package io.github.binaryfoo.lagotto.reader

import java.io.{InputStream, FilterInputStream}

import io.github.binaryfoo.lagotto.{NullProgressMeter, ProgressMeter}

class ProgressInputStream(in: InputStream, progressMeter: ProgressMeter = NullProgressMeter, val sourceName: String = "") extends FilterInputStream(in) {

  private var bytesRead = 0L
  private var recordCount = 0

  def offset: Long = bytesRead

  // somewhat assuming that we're gonna start reading right now
  if (sourceName != "")
    progressMeter.startFile(sourceName)

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

  def publishProgress(done: Boolean) = {
    if (done) {
      progressMeter.finishFile(recordCount, offset)
    } else {
      recordCount += 1
      if (recordCount % 100000 == 0) {
        progressMeter.progressInFile(recordCount, offset)
        recordCount = 0
      }
    }
  }
}
