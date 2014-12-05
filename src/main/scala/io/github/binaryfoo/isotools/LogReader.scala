package io.github.binaryfoo.isotools

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.io.{Source, BufferedSource}

object LogReader {

  def read(files: Iterable[File]): Iterable[LogEntry] = {
    var joined: Iterable[LogEntry] = List()
    for (f <- files) {
      joined = joined ++ read(Source.fromFile(f))
    }
    joined
  }

  def read(file: File*): Iterable[LogEntry] = read(file.toIterable)

  def read(source: BufferedSource): Iterable[LogEntry] = {
    val entries = new ListBuffer[LogEntry]
    var record: ListBuffer[String] = null
    for (line <- source.getLines()) {
      if (line startsWith "<log ") {
        record = new ListBuffer[String]
      }

      if (record != null)
        record += line

      if (line startsWith "</log>") {
        entries += LogEntry.fromLines(record)
        record = null
      }
    }
    entries
  }

}
