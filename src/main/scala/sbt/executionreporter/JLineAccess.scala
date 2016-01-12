package sbt.executionreporter

import sbt.JLine

/** Helper class to access sbt's JLine instance */
object JLineAccess {
  def terminalWidth: Int = JLine.usingTerminal(_.getWidth)
}