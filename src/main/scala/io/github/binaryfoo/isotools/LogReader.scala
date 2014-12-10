package io.github.binaryfoo.isotools

import java.io.{BufferedInputStream, FileInputStream, File}
import java.util.zip.GZIPInputStream

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
      joined = joined #::: read(open(f), f.getName)
    }
    joined
  }

  private def open(f: File): BufferedSource = {
    if (f.getName.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(f))))
    else
      Source.fromFile(f)
  }

  def read(source: Source, sourceName: String = ""): Stream[LogEntry] = {

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

}
