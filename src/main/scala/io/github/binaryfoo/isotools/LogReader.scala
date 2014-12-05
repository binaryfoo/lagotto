package io.github.binaryfoo.isotools

import java.io.File

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}

object LogReader {

  def readFilesOrStdIn(args: Iterable[String]): Stream[LogEntry] = {
    if (args.isEmpty)
      LogReader.read(new BufferedSource(System.in))
    else
      LogReader.read(args.map(new File(_)))
  }

  def read(files: Iterable[File]): Stream[LogEntry] = {
    var joined: Stream[LogEntry] = Stream.empty
    for (f <- files.toList) {
      joined = joined #::: read(Source.fromFile(f))
    }
    joined
  }

  def read(file: File*): Stream[LogEntry] = read(file.toIterable)

  def read(source: BufferedSource): Stream[LogEntry] = {

    val lines = source.getLines()

    def readNext(): Stream[LogEntry] = {

      var record: ListBuffer[String] = null
      var entry: LogEntry = null

      for (line <- lines) {
        if (line contains "<log ") {
          record = new ListBuffer[String]
        }

        if (record != null)
          record += line

        if (line contains "</log>") {
          entry = LogEntry.fromLines(record)
          return entry #:: readNext()
        }
      }

      Stream.empty
    }

    readNext()
  }

}
