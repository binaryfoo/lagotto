package io.github.binaryfoo.lagotto.reader

import java.io._

class TailInputStream(val file: FileInProgress) extends InputStream {

  private var raf = open
  private var bytesRead = 0L

  override def read(): Int = readOrWait { raf.read() }

  override def read(b: Array[Byte], off: Int, len: Int): Int = readOrWait { raf.read(b, off, len) }

  private def readOrWait(fn: => Int): Int = {
    var b = 0
    do {
      b = fn
      if (b != -1) {
        bytesRead += b
      } else {
        waitForMore()
      }
    } while (b == -1 && !file.done)
    b
  }

  private def waitForMore() = {
    while (file.file.length() <= bytesRead && !file.done) {
      Thread.sleep(300)
    }
    if (fileHasBeenReplaced)
      raf = open
  }

  private def fileHasBeenReplaced = file.file.length() > raf.length()
  private def open = new RandomAccessFile(file.file, "r")
}

object TailInputStream {
  def apply(file: String): TailInputStream = apply(new File(file))
  def apply(file: File): TailInputStream = new TailInputStream(new FileInProgress(file))
}

class FileInProgress(val file: File, var done: Boolean = false) {
  def open(): InputStream = {
    if (done)
      new FileInputStream(file)
    else
      new TailInputStream(this)
  }
}