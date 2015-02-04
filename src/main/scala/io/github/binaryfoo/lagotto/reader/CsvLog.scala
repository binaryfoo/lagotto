package io.github.binaryfoo.lagotto.reader

import java.util.concurrent.atomic.AtomicReference

import io.github.binaryfoo.lagotto.{SimpleLogEntry, TimeExpression}

import scala.collection.mutable

/**
 * Stateful. Not thread safe. Stores header when it sees line 1.
 */
object CsvLog extends LogType[SimpleLogEntry] {

  override type P = TextAndSource

  private val header: AtomicReference[Header] = new AtomicReference[Header]()

  override def readLinesForNextRecord(it: SourceLineIterator): TextAndSource = {
    if (it.lineNumber == 0 && it.hasNext) {
      val fields = it.next().split(',')
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
    fields ++= headerFields.zip(s.text.split(','))
    SimpleLogEntry(fields, timeFormat, s.text, s.source)
  }

  case class Header(fields: Seq[String], timeFormat: Option[TimeExpression])
}
