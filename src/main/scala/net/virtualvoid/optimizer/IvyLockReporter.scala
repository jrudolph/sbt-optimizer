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

package net.virtualvoid.optimizer

import java.io.File
import java.util.concurrent.Callable

import sbt.Keys
import xsbti._

object IvyLockReporter {
  case class SpentTimeInLock(lockFile: File, startNanos: Long, endNanos: Long) extends Span
  trait Listener {
    def spentTimeInLock(spent: SpentTimeInLock): Unit
  }

  val listener = new ThreadLocal[Listener]

  def withListener[T](l: Listener)(body: ⇒ T): T = {
    val oldListener = listener.get
    listener.set(l)
    try body
    finally listener.set(oldListener)
  }

  def locked(spent: SpentTimeInLock): Unit = {
    val l = listener.get
    if (l ne null) l.spentTimeInLock(spent)
  }

  def install() =
    Keys.appConfiguration in sbt.Global ~= { oldApp ⇒
      new AppConfiguration {
        def arguments(): Array[String] = oldApp.arguments()
        def baseDirectory(): File = oldApp.baseDirectory()
        def provider(): AppProvider = new AppProvider {
          def newMain(): AppMain = oldApp.provider.newMain
          def components(): ComponentProvider = oldApp.provider.components
          def entryPoint(): Class[_] = oldApp.provider.entryPoint
          def scalaProvider(): ScalaProvider = new ScalaProvider {
            val oldProvider = oldApp.provider.scalaProvider
            def launcher(): Launcher = new Launcher {
              val oldLauncher = oldProvider.launcher
              def globalLock(): GlobalLock = new GlobalLock {
                def apply[T](lockFile: File, run: Callable[T]): T = {
                  val start = System.nanoTime()
                  oldLauncher.globalLock.apply(lockFile, new Callable[T] {
                    def call(): T = {
                      val end = System.nanoTime()
                      IvyLockReporter.locked(IvyLockReporter.SpentTimeInLock(lockFile, start, end))
                      run.call()
                    }
                  })
                }
              }

              def isOverrideRepositories: Boolean = oldLauncher.isOverrideRepositories
              def ivyHome(): File = oldLauncher.ivyHome
              def getScala(version: String): ScalaProvider = oldLauncher.getScala(version)
              def getScala(version: String, reason: String): ScalaProvider = oldLauncher.getScala(version, reason)
              def getScala(version: String, reason: String, scalaOrg: String): ScalaProvider = oldLauncher.getScala(version, reason, scalaOrg)
              def topLoader(): ClassLoader = oldLauncher.topLoader
              def ivyRepositories(): Array[Repository] = oldLauncher.ivyRepositories
              def app(id: ApplicationID, version: String): AppProvider = oldLauncher.app(id, version)
              def checksums(): Array[String] = oldLauncher.checksums
              def appRepositories(): Array[Repository] = oldLauncher.appRepositories
              def bootDirectory(): File = oldLauncher.bootDirectory
            }

            def compilerJar(): File = oldProvider.compilerJar
            def app(id: ApplicationID): AppProvider = oldProvider.app(id)
            def loader(): ClassLoader = oldProvider.loader
            def libraryJar(): File = oldProvider.libraryJar
            def version(): String = oldProvider.version
            def jars(): Array[File] = oldProvider.jars
          }
          def loader(): ClassLoader = oldApp.provider.loader
          def mainClasspath(): Array[File] = oldApp.provider.mainClasspath
          def mainClass(): Class[_ <: AppMain] = oldApp.provider.mainClass
          def id(): ApplicationID = oldApp.provider.id
        }
      }
    }
}

