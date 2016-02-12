/*
 *    Copyright 2016 Johannes Rudolph
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 */

package sbt.executionreporter

import java.util.concurrent.ConcurrentHashMap

import net.virtualvoid.optimizer._
import IvyLockReporter.{SpentTimeInLock, Listener}
import ExecutionProgressReporter.TaskTiming

import sbt._

import scala.annotation.tailrec

private[sbt] class RichExecutionProgress extends ExecuteProgress[Task] {
  private[this] val calledBy = new ConcurrentHashMap[Task[_], Task[_]]
  private[this] val anonOwners = new ConcurrentHashMap[Task[_], Task[_]]
  private[this] val data = new ConcurrentHashMap[Task[_], TaskTiming]

  type S = Unit
  def initial: S = {
    data.clear()
    calledBy.clear()
    anonOwners.clear()
  }
  def registered(state: S, task: Task[_], allDeps: Iterable[Task[_]], pendingDeps: Iterable[Task[_]]): S = {
    pendingDeps foreach { t ⇒ if (TaskName.transformNode(t).isEmpty) anonOwners.put(t, task) }
    if (data.containsKey(task))
      updateData(task)(d ⇒ d.copy(task, deps = d.deps ++ allDeps.toSeq, registerTime = theTime()))
    else
      data.put(task, TaskTiming(task, mappedName(task), deps = allDeps.toSeq, registerTime = theTime()))
  }

  def ready(state: S, task: Task[_]): S = updateData(task)(_.copy(readyTime = theTime()))
  def workStarting(task: Task[_]): Unit = {
    val listener = new DownloadListener {
      def downloadOccurred(download: NetworkAccess): Unit = updateData(task)(d ⇒ d.copy(downloads = d.downloads :+ download))
    }
    val lockListener = new Listener {
      def spentTimeInLock(spent: SpentTimeInLock): Unit = updateData(task)(d ⇒ d.copy(locks = d.locks :+ spent))
    }
    require(IvyDownloadReporter.listener.get == null)
    IvyDownloadReporter.listener.set(listener)
    IvyLockReporter.listener.set(lockListener)
    updateData(task)(_.copy(startTime = theTime(), threadId = Thread.currentThread().getId))
  }

  def workFinished[T](task: Task[T], result: Either[Task[T], Result[T]]): Unit = {
    IvyDownloadReporter.listener.set(null)
    IvyLockReporter.listener.set(null)
    updateData(task)(_.copy(finishTime = theTime()))
    result.left.foreach { t ⇒
      calledBy.put(t, task)
      data.put(t, TaskTiming(t, mappedName(t), deps = Seq(task)))
    }
  }
  def completed[T](state: S, task: Task[T], result: Result[T]): S = updateData(task)(_.copy(completeTime = theTime()))
  def allCompleted(state: S, results: RMap[Task, Result]): S = ExecutionProgressReporter.report(data)

  def theTime(): Option[Long] = Some(System.nanoTime())

  @tailrec
  private[this] def updateData(task: Task[_])(f: TaskTiming ⇒ TaskTiming): Unit = {
    val old = data.get(task)
    val newValue = f(old)
    if (!data.replace(task, old, newValue)) updateData(task)(f)
  }

  private[this] def inferredName(t: Task[_]): Option[String] =
    Option(anonOwners.get(t)).map(t ⇒ mappedName(t) + "<anon>") orElse
      Option(calledBy.get(t)).map(t ⇒ mappedName(t) + "<called-by>")
  var cache = Map.empty[Task[_], String]
  private[this] def mappedName(t: Task[_]): String = {
    cache.get(t) match {
      case Some(name) ⇒ name
      case None ⇒
        cache = cache.updated(t, "<cyclic>")
        val name = mappedNameImpl(t)
        cache = cache.updated(t, name)
        name
    }
  }
  private[this] def mappedNameImpl(t: Task[_]): String = TaskName.definedName(t) orElse inferredName(t) getOrElse TaskName.anonymousName(t)
}

object RichExecutionProgress {
  val install: sbt.Def.Setting[_] = Keys.executeProgress := (_ ⇒ new Keys.TaskProgress(new RichExecutionProgress))
}
