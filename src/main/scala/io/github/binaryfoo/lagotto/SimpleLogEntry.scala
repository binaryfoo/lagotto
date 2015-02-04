package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.mutable

/**
 * Ceremony around a Map.
 */
case class SimpleLogEntry(private val _fields: mutable.LinkedHashMap[String, String], private val timeFormat: Option[TimeExpression] = None, lines: String, source: SourceRef = null) extends LogEntry {

  val timestamp: DateTime = {
    timeFormat.map { case TimeExpression(expr, formatter) =>
      _fields.get(expr)
      .map(formatter.parseDateTime)
      .getOrElse(throw new IAmSorryDave(s"Missing 'timestamp' in ${_fields}"))
    }.orNull
  }

  val fields = _fields.withDefault {
    case TimeFormatter(format) => format.print(timestamp)
    case "file" if source != null => source.toString
    case "line" if source != null => source.line.toString
    case _ => null
  }

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}