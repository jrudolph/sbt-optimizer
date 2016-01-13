package net.virtualvoid.optimizer

import java.io.{ FileWriter, File }
import java.util.concurrent.ConcurrentHashMap

import net.virtualvoid.optimizer.ExecutionProgressReporter.TaskTiming
import sbt.Task

object CatapultTraceEventWriter {
  val ThePid = 12345

  def writeTo(file: File, dataMap: ConcurrentHashMap[Task[_], TaskTiming]): Unit = {
    import scala.collection.JavaConverters._
    val timings = dataMap.values().asScala.toVector

    case class CompleteEvent(label: String, pid: Long, tid: Long, startMicros: Long, durationMicros: Long)

    def eventsOf(timing: TaskTiming): Seq[CompleteEvent] = {
      val mainEvent =
        CompleteEvent(
          timing.taskName,
          ThePid,
          timing.threadId,
          timing.startTime.get / 1000,
          timing.workTime.get / 1000)
      val downloadEvents =
        timing.downloads.map {
          case a @ NetworkAccess(url, kind, startNanos, endNanos) ⇒
            CompleteEvent(
              s"$kind: $url",
              ThePid,
              timing.threadId,
              startNanos / 1000,
              a.lasted / 1000)
        }
      val globalLockEvents =
        timing.locks.map {
          case l @ IvyLockReporter.SpentTimeInLock(lock, startNanos, end) ⇒
            CompleteEvent(
              "Ivy locked",
              ThePid,
              timing.threadId,
              startNanos / 1000,
              l.lasted / 1000)
        }

      (mainEvent +: downloadEvents) ++ globalLockEvents
    }

    def printEvent(event: CompleteEvent): String = {
      import event._
      s"""{
         |  "name": "$label",
         |  "ph": "X",
         |  "ts": $startMicros,
         |  "dur": $durationMicros,
         |  "pid": $pid,
         |  "tid": $tid
         |}
       """.stripMargin
    }

    val events = timings.flatMap(eventsOf).map(printEvent)

    val writer = new FileWriter(file)
    writer.write(
      s"""{
         |    "traceEvents": [
         |      ${events.mkString(",\n")}
         |    ]
         |}
       """.stripMargin)
    writer.close()
  }
}