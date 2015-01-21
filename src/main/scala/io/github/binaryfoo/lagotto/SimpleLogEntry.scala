package io.github.binaryfoo.lagotto

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormatter

import scala.collection.mutable

/**
 * Ceremony around a Map.
 */
case class SimpleLogEntry(private val _fields: mutable.LinkedHashMap[String, String], private val timeFormat: DateTimeFormatter, lines: String, source: SourceRef = null) extends LogEntry {

  val fields = _fields.withDefault {
    case TimeFormatter(format) => format.print(timestamp)
    case _ => null
  }

  lazy val timestamp: DateTime = {
    _fields.get("timestamp")
      .map(timeFormat.parseDateTime)
      .getOrElse(throw new IAmSorryDave(s"Missing 'timestamp' in ${_fields}"))
  }

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}