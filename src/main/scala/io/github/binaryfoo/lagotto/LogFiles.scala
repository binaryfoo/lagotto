package io.github.binaryfoo.lagotto

import java.io.{FilenameFilter, File}

object LogFiles {

  val LogSequenceNumber = """.*\.(\d{1,})\.log(?:\.gz)?""".r

  val endsWithLog = new FilenameFilter {
    override def accept(dir: File, name: String): Boolean = name.endsWith(".log")
  }

  def file(name: String) = new File(name)

  def sequenceNumber(f: File): Int = sequenceNumber(f.getName)

  def sequenceNumber(f: String): Int = f match {
    case LogSequenceNumber(n) => n.toInt
    case _ => 0
  }

  def logsFrom(dir: String) = {
    new File(dir).listFiles(endsWithLog).sortBy(sequenceNumber)
  }
}
