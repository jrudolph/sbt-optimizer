package net.virtualvoid.optimizer

import java.io.{ FileWriter, File }
import java.util.concurrent.ConcurrentHashMap

import net.virtualvoid.optimizer.ExecutionProgressReporter.TaskTiming
import sbt.Task

object DotOutput {
  def writeTo(file: File, dataMap: ConcurrentHashMap[Task[_], TaskTiming]): Unit = {
    import scala.collection.JavaConverters._
    val timings = dataMap.values().asScala.toVector

    def node(timing: TaskTiming): String = s""""${timing.taskName}""""
    def edge(from: TaskTiming, to: TaskTiming): String =
      s""""${from.taskName}"->"${to.taskName}""""

    val nodes = timings.map(node)
    val edges = timings.flatMap(from ⇒ from.deps.map(to ⇒ edge(from, dataMap.get(to))))

    val writer = new FileWriter(file)
    writer.write(
      s"""digraph "dependency-graph" {
         |    graph[rankdir="LR"]
         |    edge [
         |        arrowtail="none"
         |    ]
         |    ${nodes.mkString("\n")}
         |    ${edges.mkString("\n")}
         |}
       """.stripMargin)
    writer.close()
  }
}
