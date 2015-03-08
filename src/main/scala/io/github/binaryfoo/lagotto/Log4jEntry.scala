package io.github.binaryfoo.lagotto

import java.lang.Integer.parseInt

import org.joda.time.DateTime
import org.joda.time.chrono.ISOChronology

import scala.collection.mutable

case class Log4jEntry(private val _fields: mutable.LinkedHashMap[String, String], lines: String, source: SourceRef = null) extends LogEntry {

  val timestamp: DateTime = {
    _fields.get("timestamp")
      .map(Log4jEntry.parseDateTime)
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
    case id => nested.map(_(id)).orNull
  }

  def level = _fields("level")
  def realm = _fields("category")
  def message = _fields("message")

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

object Log4jEntry {

  private val chronology = ISOChronology.getInstance()

  def parseDateTime(s: String): DateTime = {
    // Avoid DateTimeFormat.forPattern("dd MMM yyyy HH:mm:ss,SSS") for speed with N million+ entries

    val dayEnd = s.indexOf(' ')
    val monthEnd = s.indexOf(' ', dayEnd + 1)
    val yearEnd = s.indexOf(' ', monthEnd + 1)
    val hourEnd = s.indexOf(':', yearEnd + 1)
    val minuteEnd = s.indexOf(':', hourEnd + 1)
    val secondEnd = s.indexOf(',', minuteEnd + 1)

    val day = parseInt(s.substring(0, dayEnd))
    val month = EnglishMonths.months(s.substring(dayEnd + 1, monthEnd))
    val year = parseInt(s.substring(monthEnd + 1, yearEnd))
    val hour = parseInt(s.substring(yearEnd + 1, hourEnd))
    val minute = parseInt(s.substring(hourEnd + 1, minuteEnd))
    val second = parseInt(s.substring(minuteEnd + 1, secondEnd))
    val millis = parseInt(s.substring(secondEnd + 1))

    new DateTime(year, month, day, hour, minute, second, millis, chronology)
  }

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
