package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.mutable

/**
 * Ceremony around a Map.
 */
case class SimpleLogEntry(private val _fields: mutable.LinkedHashMap[String, String], private val timeFormat: Option[TimeExpr] = None, lines: String, source: SourceRef = null) extends LogEntry {

  val timestamp: DateTime = {
    timeFormat.map { case TimeExpr(expr, formatter) =>
      _fields.get(expr)
      .map(formatter.parseDateTime)
      .getOrElse(throw new IAmSorryDave(s"Missing 'timestamp' in ${_fields}"))
    }.orNull
  }

  val fields = _fields.withDefaultValue(null)

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

object SimpleLogEntry {
  def apply(lines: String, fields: (String, String)*): SimpleLogEntry = SimpleLogEntry(mutable.LinkedHashMap(fields :_*), lines = lines)
  def apply(fields: (String, String)*): SimpleLogEntry = SimpleLogEntry(mutable.LinkedHashMap(fields :_*), lines = "")
}