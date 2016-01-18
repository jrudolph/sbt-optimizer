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