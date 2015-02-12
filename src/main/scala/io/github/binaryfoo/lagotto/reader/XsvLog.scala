package io.github.binaryfoo.lagotto.reader

import java.util.concurrent.atomic.AtomicReference

import io.github.binaryfoo.lagotto.{XsvLogEntry, SimpleLogEntry, TimeExpr}

import scala.collection.mutable

/**
 * Stateful. Not thread safe. Stores header when it sees line 1.
 */
class XsvLog(val delimiter: Char = ',', val hasHeader: Boolean = true) extends LogType[XsvLogEntry] {

  override type P = TextAndSource

  private val header: AtomicReference[Header] = new AtomicReference[Header]()

  override def readLinesForNextRecord(it: LineIterator): TextAndSource = {
    if (it.lineNumber == 0 && it.hasNext) {
      header.set(readHeader(it))
    }
    val line = if (it.hasNext) it.next() else ""
    if (line.nonEmpty) {
      new TextAndSource(line, it.sourceRef)
    } else {
      null.asInstanceOf[TextAndSource]
    }
  }

  private def readHeader(it: LineIterator): Header = {
    if (hasHeader) {
      val fields = split(it.next())
      val timeFormat = fields.collectFirst {
        case TimeExpr(formatter) => formatter
      }
      Header(fields, timeFormat)
    } else {
      val indices = 0 to split(it.head).size - 1
      Header(indices.map(_.toString), None)
    }
  }

  override def parse(s: TextAndSource): XsvLogEntry = {
    val Header(headerFields, timeFormat) = header.get()
    val fields = new mutable.LinkedHashMap[String, String]()
    fields ++= headerFields.zip(split(s.text))
    XsvLogEntry(SimpleLogEntry(fields, timeFormat, s.text, s.source), delimiter)
  }

  @inline
  private def split(line: String) = line.split(delimiter)

  case class Header(fields: Seq[String], timeFormat: Option[TimeExpr])
}
