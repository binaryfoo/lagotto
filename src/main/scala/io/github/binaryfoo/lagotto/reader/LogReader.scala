package io.github.binaryfoo.lagotto.reader

import java.io.{BufferedInputStream, File, FileInputStream}
import java.util.zip.GZIPInputStream

import io.github.binaryfoo.lagotto.{SourceRef, LogEntry, NullProgressMeter, ProgressMeter}

import scala.collection.AbstractIterator
import scala.io.{BufferedSource, Source}

case class LogReader[T <: LogEntry](strict: Boolean = false, keepFullText: Boolean = true, progressMeter: ProgressMeter = NullProgressMeter, logType: LogType[T] = JposLog) {

  def readFilesOrStdIn(args: Iterable[String]): Iterator[T] = {
    if (args.isEmpty)
      read(new BufferedSource(System.in))
    else
      read(args.map(new File(_)))
  }

  def read(file: File*): Iterator[T] = read(file.toIterable)

  def read(files: Iterable[File]): Iterator[T] = {
    progressMeter.startRun(files.size)
    files.toIterator.flatMap(f => read(open(f), f.getName))
  }

  private def open(f: File): BufferedSource = {
    if (f.getName.endsWith(".gz"))
      Source.fromInputStream(new GZIPInputStream(new BufferedInputStream(new FileInputStream(f))))
    else
      Source.fromFile(f)
  }

  def read(source: Source, sourceName: String = ""): Iterator[T] = new LogEntryIterator(source, sourceName, progressMeter)

  class LogEntryIterator(source: Source, sourceName: String = "", progressMeter: ProgressMeter) extends AbstractIterator[T] {

    private val lines = new SourceLineIterator(source.getLines(), sourceName, strict, keepFullText)
    private var recordCount = 0
    private var current = readNext()

    override def hasNext: Boolean = current != null

    override def next(): T = {
      val v = current
      current = readNext()
      v
    }

    if (sourceName != "")
      progressMeter.startFile(sourceName)

    def readNext(): T = {
      val entry = logType(lines)
      if (entry != null) {
        recordCount += 1
      } else {
        progressMeter.finishFile(recordCount)
        source.close()
      }
      entry
    }

  }

}

/**
 * Adds a line number, name and a single line push back over Source.getLines().
 */
class SourceLineIterator(val lines: Iterator[String], val sourceName: String, val strict: Boolean, val keepFullText: Boolean) extends AbstractIterator[String] {

  private var lineNo = 0
  private var sleeve: Option[String] = None

  def lineNumber: Int = lineNo
  def sourceRef: SourceRef = SourceRef(sourceName, lineNo)

  def hasNext = sleeve.isDefined || lines.hasNext

  def next() = {
    lineNo += 1
    if (sleeve.isDefined) {
      val line = sleeve.get
      sleeve = None
      line
    } else {
      lines.next()
    }
  }

  def peek() = {
    val line = next()
    pushBack(line)
    line
  }

  def pushBack(line: String) = {
    if (sleeve.isDefined)
      throw new IllegalStateException(s"Already one line pushed back '${sleeve.get}'")
    sleeve = Some(line)
    lineNo -= 1
  }
}
