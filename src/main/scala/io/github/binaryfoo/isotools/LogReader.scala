package io.github.binaryfoo.isotools

import java.io.File
import java.nio.ByteBuffer

import io.github.binaryfoo.isotools.mmap.LiteString

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}

object LogReader {

  def readFilesOrStdIn(args: Iterable[String]): Stream[LogEntry] = {
    if (args.isEmpty)
      LogReader.read(new BufferedSource(System.in))
    else
      LogReader.read(args.map(new File(_)))
  }

  def read(file: File*): Stream[LogEntry] = read(file.toIterable)

  def read(files: Iterable[File]): Stream[LogEntry] = {
    var joined: Stream[LogEntry] = Stream.empty
    for (f <- files.toList) {
      joined = joined #::: read(Source.fromFile(f), f.getName)
    }
    joined
  }

  def read(source: BufferedSource, sourceName: String = ""): Stream[LogEntry] = {

    val lines = source.getLines()
    var startLineNumber = 0
    var lineNumber = 0

    def readNext(): Stream[LogEntry] = {

      var record: ListBuffer[String] = null
      var entry: LogEntry = null

      for (line <- lines) {
        lineNumber += 1

        if (line.contains("<log ") && record == null) {
          // TODO: collect warning if record != null
          startLineNumber = lineNumber
          record = new ListBuffer[String]
        }

        if (record != null)
          record += line

        if (line.contains("</log>") && record != null) {
          // TODO: collect warning if record == null
          try {
            entry = LogEntry.fromLines(record, SourceRef(sourceName, startLineNumber))
            return entry #:: readNext()
          }
          catch {
            case e: IllegalArgumentException =>
              // TODO: collect warning
              record = null
          }
        }
      }

      Stream.empty
    }

    readNext()
  }

  def readMapped(source: ByteBuffer, sourceName: String = ""): Stream[LogEntry] = {

    val rootString = LiteString.untilEnd(source)
    var lines = rootString.split('\n')
    var startLineNumber = 0
    var lineNumber = 0

    def readNext(): Stream[LogEntry] = {

      var firstLine: LiteString = null
      var entry: LogEntry = null

      while (lines != Stream.empty) {
        val line = lines.head
        lines = lines.tail
        lineNumber += 1

        if (line.contains("<log ") && firstLine == null) {
          // TODO: collect warning if firstLine != null
          startLineNumber = lineNumber
          firstLine = line
        }

        if (line.contains("</log>") && firstLine != null) {
          // TODO: collect warning if record == null
          try {
            val record = rootString.substring(firstLine.start, line.absoluteEnd + 1).toString
            entry = LogEntry.fromString(record, SourceRef(sourceName, startLineNumber))
            return entry #:: readNext()
          }
          catch {
            case e: IllegalArgumentException =>
              // TODO: collect warning
              firstLine = null
          }
        }
      }

      Stream.empty
    }

    readNext()
  }

  def readMappedScan(source: ByteBuffer, sourceName: String = ""): Stream[LogEntry] = {

    val rootString = LiteString.untilEnd(source)
    var currentIndex = 0

    def readNext(): Stream[LogEntry] = {

      while (true) {
        val start = rootString.indexOf("<log ", currentIndex)
        val end = if (start != -1) rootString.indexOf("</log>", start + 1) else -1
        if (end == -1) {
          return Stream.empty
        }
        currentIndex = end +1
        val line = rootString.substring(start, currentIndex)

        val entry = LogEntry.fromString(line.toString, SourceRef(sourceName, 0))
        return entry #:: readNext()
      }

      Stream.empty
    }

    readNext()
  }

}
