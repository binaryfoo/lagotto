package io.github.binaryfoo.lagotto.reader

import java.io.{InputStream, FilterInputStream}

class ProgressInputStream(in: InputStream, val length: Long = 0) extends FilterInputStream(in) {

  private var bytesRead = 0L

  def offset: Long = bytesRead

  override def read(): Int = {
    val b = super.read()
    bytesRead += 1
    b
  }

  override def read(b: Array[Byte]): Int = {
    val count = super.read(b)
    bytesRead += count
    count
  }

  override def read(b: Array[Byte], off: Int, len: Int): Int = {
    val count = super.read(b, off, len)
    bytesRead += count
    count
  }

  override def skip(n: Long): Long = {
    val skipped = super.skip(n)
    bytesRead += skipped
    skipped
  }

  override def reset(): Unit = {
    super.reset()
    bytesRead = 0
  }
}
