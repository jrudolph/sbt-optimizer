package net.virtualvoid.optimizer

abstract class Span {
  def startNanos: Long
  def endNanos: Long

  def lasted: Long = endNanos - startNanos
  def lastedMillis = lasted / 1000000
}