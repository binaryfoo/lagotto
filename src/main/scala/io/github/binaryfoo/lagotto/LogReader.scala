package io.github.binaryfoo.lagotto

import java.io.{BufferedInputStream, FileInputStream, File}
import java.util.zip.GZIPInputStream

import scala.collection.AbstractIterator
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}

case class LogReader(strict: Boolean = false, keepFullText: Boolean = true, progressMeter: ProgressMeter = NullProgressMeter) {

  def readFilesOrStdIn(args: Iterable[String]): Iterator[LogEntry] = {
    if (args.isEmpty)
      read(new BufferedSource(System.in))
    else
      read(args.map(new File(_)))
  }

  def read(file: File*): Iterator[LogEntry] = read(file.toIterable)

  def read(files: Iterable[File]): Iterator[LogEntry] = {
    progressMeter.startRun(files.size)
    files.toIterator.flatMap(f => read(open(f), f.getName))
  }

  private def open(f: File): BufferedSource = {
    if (f.getName.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(f))))
    else
      Source.fromFile(f)
  }

  class LogEntryIterator(source: Source, sourceName: String = "") extends AbstractIterator[LogEntry] {

    private val lines = source.getLines()
    private var startLineNumber = 0
    private var lineNumber = 0
    private var recordCount = 0
    private var current = readNext()

    override def hasNext: Boolean = current != null

    override def next(): LogEntry = {
      val v = current
      current = readNext()
      v
    }

    if (sourceName != "")
      progressMeter.startFile(sourceName)

    def readNext(): LogEntry = {

      var record: ListBuffer[String] = null

      for (line <- lines) {
        lineNumber += 1

        if (line.contains("<log ")) {
          if (record == null) {
            startLineNumber = lineNumber
            record = new ListBuffer[String]
          } else if (strict) {
            throw new IllegalArgumentException(s"Unexpected <log> start tag. Line $sourceName:$lineNumber: $line")
          }
        }

        if (record != null)
          record += line

        if (line.contains("</log>")) {
          if (record != null) {
            try {
              val fullText = if (keepFullText) record.mkString("\n") else ""
              recordCount += 1
              return LogEntry(LogEntry.extractFields(record), fullText, SourceRef(sourceName, startLineNumber))
            }
            catch {
              case e: IllegalArgumentException =>
                if (strict) {
                  throw new IllegalArgumentException(s"Failed to process record ending line $sourceName:$lineNumber", e)
                }
                record = null
            }

          } else if (strict) {
            throw new IllegalArgumentException(s"Unexpected </log> end tag. Line $sourceName:$lineNumber: $line")
          }
        }
      }

      progressMeter.finishFile(recordCount)

      source.close()

      null
    }

  }

  def read(source: Source, sourceName: String = ""): Iterator[LogEntry] = new LogEntryIterator(source, sourceName)

}
