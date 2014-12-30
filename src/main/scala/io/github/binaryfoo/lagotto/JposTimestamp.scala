package io.github.binaryfoo.lagotto

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object JposTimestamp {

  private val FORMAT = DateTimeFormat.forPattern("EEE MMM dd HH:mm:ss zzz yyyy")
  private val READ_WORKAROUND_FORMAT = DateTimeFormat.forPattern("MMM dd HH:mm:ss yyyy")

  def parse(s: String): DateTime = {
    try {
      val dot = s.lastIndexOf('.')
      val millis = if (dot == -1) 0 else s.substring(dot + 1).toInt
      val end = if (dot == -1) s.length else dot
      READ_WORKAROUND_FORMAT.parseDateTime(s.substring(4, end).replace("EST ", "")).withMillisOfSecond(millis)
    }
    catch {
      case e: Throwable => throw new IllegalArgumentException(s"Failed to parse time: $s", e)
    }
  }

  def format(d: DateTime): String = d.toString(FORMAT) + "." + d.getMillisOfSecond

  implicit class DateTimeExtension(val d: DateTime) extends AnyVal {
    def asJposAt: String = format(d)
  }
}
