package io.github.binaryfoo.lagotto

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.collection.mutable

case class Log4jEntry(private val _fields: mutable.LinkedHashMap[String, String], lines: String, source: SourceRef = null) extends LogEntry {

  val timestamp: DateTime = {
    _fields.get("timestamp")
      .map(Log4jEntry.TimeFormat.parseDateTime)
      .getOrElse(throw new IAmSorryDave(s"Missing 'timestamp' in ${_fields}"))
  }

  _fields.put("timestamp", DefaultDateTimeFormat.print(timestamp))

  val nested: Option[JposEntry] = {
    val jposStart = lines.indexOf("<log ")
    Option(if (jposStart != -1) {
      JposEntry.fromString(lines.substring(jposStart), source)
    } else {
      null
    })
  }

  val fields = _fields.withDefault {
    case TimeFormatter(format) => format.print(timestamp)
    case id => nested.flatMap(jPos => jPos.get(id).orElse{ id match {
      case Log4jEntry.JposAccess(path) => jPos.get(path)
      case _ => None
    }}).orNull
  }

  def level = _fields("level")
  def realm = _fields("category")
  def message = _fields("message")

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

object Log4jEntry {

  val TimeFormat = DateTimeFormat.forPattern("dd MMM yyyy HH:mm:ss,SSS")
  val JposAccess = """jpos\.(.+)""".r

  def fromString(s: String, source: SourceRef = null): Log4jEntry = {
    // avoid a regex to save some time
    val timeEnd = s.indexOf(']', 1)
    val levelEnd = s.indexOf(' ', timeEnd + 2)
    val categoryEnd = s.indexOf(']', levelEnd + 3)

    Log4jEntry(mutable.LinkedHashMap(
      "timestamp" -> s.substring(1, timeEnd),
      "level" -> s.substring(timeEnd + 2, levelEnd),
      "category" -> s.substring(levelEnd + 3, categoryEnd),
      "message" -> s.substring(categoryEnd + 3)
    ), s, source)
  }

}
