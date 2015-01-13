package io.github.binaryfoo.lagotto

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import scala.collection.mutable

case class Log4jEntry(private val _fields: mutable.LinkedHashMap[String, String], lines: String, source: SourceRef = null) extends LogLike {

  val fields = _fields.withDefault {
    case TimeFormatter(format) => format.print(timestamp)
    case _ => null
  }

  lazy val timestamp: DateTime = {
    _fields.get("timestamp")
      .map(Log4jEntry.TimeFormat.parseDateTime)
      .getOrElse(throw new IAmSorryDave(s"Missing 'timestamp' in ${_fields}"))
  }

  def level = _fields("level")
  def realm = _fields("realm")
  def message = _fields("message")

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

object Log4jEntry {

  val Record = """(?s)\[([^]]+)\] (\w+) +\[([^]]+)\]: (.*)""".r
  val TimeFormat = DateTimeFormat.forPattern("dd MMM yyyy HH:mm:ss,SSS")

  def fromString(s: String, source: SourceRef = null): Log4jEntry = {
    s match {
      case Record(time, level, realm, message) =>
        Log4jEntry(mutable.LinkedHashMap(
          "timestamp" -> time,
          "level" -> level,
          "realm" -> realm,
          "message" -> message
        ), s, source)
      case s => throw new IllegalArgumentException(s"Not a log4j record: '$s'")
    }
  }

}

object Log4jReader {

}
