package io.github.binaryfoo.lagotto.reader

import java.util.concurrent.atomic.AtomicReference

import io.github.binaryfoo.lagotto.{SimpleLogEntry, TimeExpression}

import scala.collection.mutable

/**
 * Stateful. Not thread safe. Stores header when it sees line 1.
 */
class XsvLog(val delimiter: Char = ',') extends LogType[SimpleLogEntry] {

  override type P = TextAndSource

  private val header: AtomicReference[Header] = new AtomicReference[Header]()

  override def readLinesForNextRecord(it: LineIterator): TextAndSource = {
    if (it.lineNumber == 0 && it.hasNext) {
      val fields = split(it.next())
      val timeFormat = fields.collectFirst {
        case TimeExpression(formatter) => formatter
      }
      header.set(Header(fields, timeFormat))
    }
    val line = if (it.hasNext) it.next() else ""
    if (line.nonEmpty) {
      new TextAndSource(line, it.sourceRef)
    } else {
      null.asInstanceOf[TextAndSource]
    }
  }

  override def parse(s: TextAndSource): SimpleLogEntry = {
    val Header(headerFields, timeFormat) = header.get()
    val fields = new mutable.LinkedHashMap[String, String]()
    fields ++= headerFields.zip(split(s.text))
    SimpleLogEntry(fields, timeFormat, s.text, s.source)
  }

  @inline
  private def split(line: String) = line.split(delimiter)

  case class Header(fields: Seq[String], timeFormat: Option[TimeExpression])
}
