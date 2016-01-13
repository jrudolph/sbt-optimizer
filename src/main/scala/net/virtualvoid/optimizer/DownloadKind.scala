/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2016 Johannes Rudolph
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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