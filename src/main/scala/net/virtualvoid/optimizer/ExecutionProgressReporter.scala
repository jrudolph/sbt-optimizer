package net.virtualvoid.optimizer

import java.util.concurrent.ConcurrentHashMap

import sbt.Task
import sbt.executionreporter.JLineAccess

object ExecutionProgressReporter {
  def install() = sbt.executionreporter.RichExecutionProgress.install

  case class TaskTiming(
      task: Task[_],
      taskName: String,
      deps: Seq[Task[_]] = Nil,
      registerTime: Option[Long] = None,
      readyTime: Option[Long] = None,
      startTime: Option[Long] = None,
      finishTime: Option[Long] = None,
      completeTime: Option[Long] = None,
      threadId: Long = -1,
      locks: Seq[IvyLockReporter.SpentTimeInLock] = Nil,
      downloads: Seq[NetworkAccess] = Nil) {
    def workTime: Option[Long] = diff(startTime, finishTime)
    def totalTime: Option[Long] = diff(registerTime, completeTime)
    def downloadTime: Long = downloads.map(_.lasted).sum
    def lockTime: Long = locks.map(_.lasted).sum
    def pureTime: Long = workTime.map(_ - downloadTime - lockTime).get

    def diff(start: Option[Long], end: Option[Long]): Option[Long] =
      for {
        s ← start
        e ← end
      } yield e - s
  }

  def report(dataMap: ConcurrentHashMap[Task[_], TaskTiming]): Unit = {
    // report
    import scala.collection.JavaConverters._
    val datas = dataMap.elements().asScala.toVector

    val start = datas.flatMap(_.registerTime).min
    val end = datas.flatMap(_.completeTime).max
    val setup = OutputSetup(start, end)

    var visited = Set.empty[Task[_]]
    def walkTree(element: Task[_]): Unit =
      if (!visited(element)) {
        visited += element
        val d = dataMap.get(element)
        d.deps.sortBy(d ⇒ dataMap.get(d).startTime.get).foreach(walkTree)
        if (d.workTime.get > setup.nanosPerSlot / 2)
          println(line(setup)(d))
      }

    def roots: Seq[Task[_]] = {
      val children = datas.flatMap(d ⇒ d.deps).toSet
      (datas.map(_.task).toSet -- children).toSeq
    }

    println("Roots")
    roots.foreach(r ⇒ println(dataMap.get(r).taskName))

    roots.foreach(walkTree)

    val networkAccesses = datas.flatMap(_.downloads)
    if (networkAccesses.nonEmpty) IvyDownloadReporter.printTracingReport(networkAccesses)
  }

  case class Slot(start: Long, end: Long) {
    def middle = (start + end) / 2
    def activeDuring(start: Option[Long], end: Option[Long]): Boolean =
      middle >= start.get && middle < end.get
  }
  case class OutputSetup(
      startTime: Long,
      endTime: Long,
      timeWidth: Int = 9,
      pureTimeWidth: Int = 10,
      lockTimeWidth: Int = 10,
      downloadsWidth: Int = 12,
      nameWidth: Int = 30) {
    val terminalWidth = JLineAccess.terminalWidth
    val totalNanos: Long = endTime - startTime
    val numberOfSlots = terminalWidth - timeWidth - pureTimeWidth - lockTimeWidth - downloadsWidth - nameWidth - 3 /* space */ - 2 /* brackets */
    val nanosPerSlot = totalNanos / numberOfSlots
    val slots = (0 until numberOfSlots) map { id ⇒
      val start = startTime + nanosPerSlot * id
      val end = math.min(endTime, start + nanosPerSlot)
      Slot(start, end)
    }
  }
  sealed trait TaskState
  case object Idle extends TaskState
  case object WaitingForDependencies extends TaskState
  case object WaitingForExecutor extends TaskState
  case object WaitingForDownload extends TaskState
  case object WaitingForGlobalLock extends TaskState
  case object Running extends TaskState

  def line(setup: OutputSetup)(data: TaskTiming): String = {
    def stateAtSlot(slot: Slot): TaskState =
      if (slot.activeDuring(data.registerTime, data.readyTime)) WaitingForDependencies
      else if (slot.activeDuring(data.readyTime, data.startTime)) WaitingForExecutor
      else if (data.downloads.exists(d ⇒ slot.activeDuring(Some(d.startNanos), Some(d.endNanos)))) WaitingForDownload
      else if (data.locks.exists(d ⇒ slot.activeDuring(Some(d.startNanos), Some(d.endNanos)))) WaitingForGlobalLock
      else if (slot.activeDuring(data.startTime, data.finishTime)) Running
      else Idle

    def stateSign(state: TaskState): String = state match {
      case Idle                   ⇒ " "
      case WaitingForDependencies ⇒ scala.Console.YELLOW + "-"
      case WaitingForExecutor     ⇒ scala.Console.RED + "!"
      case WaitingForGlobalLock   ⇒ scala.Console.RED + "L"
      case Running                ⇒ scala.Console.GREEN + "#"
      case WaitingForDownload     ⇒ scala.Console.CYAN + "↓"
    }

    val shortName = data.taskName.takeRight(setup.nameWidth)
    val signs = setup.slots.map(stateAtSlot).map(stateSign).mkString

    // FIXME: put widths into format string
    f"${data.workTime.get / 1000000}%6d ms ${scala.Console.GREEN}${data.pureTime / 1000000}%6d ms ${scala.Console.RED}${data.lockTime / 1000000}%6d ms ${scala.Console.CYAN}${data.downloadTime / 1000000}%6d ms ${data.downloads.size}%2d${scala.Console.RESET} $shortName%-30s >$signs${scala.Console.RESET}>"
  }
}