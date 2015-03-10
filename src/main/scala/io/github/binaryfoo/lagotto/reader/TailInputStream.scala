package io.github.binaryfoo.lagotto.reader

import java.io._

import io.github.binaryfoo.lagotto.Debug

/**
 * Tail a log file until it's marked as done.
 */
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
    // policy is to keep waiting if:
    // 1. There's the same number of bytes on disk
    // 2. The file disappears. The assumption being some log rolling process will put a new file in its place soon.
    while (!file.done && (bytesOnDisk == bytesRead || !file.file.exists())) {
      Thread.sleep(300)
    }
    if (fileHasBeenReplaced) {
      raf.close()
      raf = open
      bytesRead = 0
    }
  }

  private def fileHasBeenReplaced = {
    val currentLengthOnDisk = bytesOnDisk
    Debug.log(s"Tail of ${file.file.getName} found lengths: open=${raf.length()}, disk=$currentLengthOnDisk, read $bytesRead}")
    currentLengthOnDisk < bytesRead || currentLengthOnDisk > raf.length()
  }

  private def bytesOnDisk = file.file.length()

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