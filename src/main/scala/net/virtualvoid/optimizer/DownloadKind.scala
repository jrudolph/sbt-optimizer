package net.virtualvoid.optimizer

import java.io.File
import java.net.URL

sealed trait DownloadKind extends Product {
  def extraInfo: String = ""
}
case object Failed extends DownloadKind
case class GetURLInfo(isAvailable: Boolean) extends DownloadKind {
  import Console._
  override def extraInfo: String =
    if (isAvailable) s"${GREEN}AVAILABLE$RESET"
    else s"${RED}MISSING$RESET"
}
case class DownloadURL(to: File, size: Long) extends DownloadKind {
  override def extraInfo: String = f"-> $to%s $size%d"
}
case object OpenStreamURL extends DownloadKind
case class NetworkAccess(url: URL, kind: DownloadKind, startNanos: Long, endNanos: Long) extends Span {
  import Console._
  def color: String = url.getProtocol match {
    case "file"           ⇒ GREEN
    case "jar"            ⇒ YELLOW
    case "http" | "https" ⇒ CYAN
    case _                ⇒ BLUE
  }

  override def toString: String =
    f"$lastedMillis%5d ms ${kind.productPrefix}%-15s $color%s$url%s$RESET ${kind.extraInfo}"
}