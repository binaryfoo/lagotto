package io.github.binaryfoo.isotools

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object JposTimestamp {

  private val FORMAT = DateTimeFormat.forPattern("EEE MMM dd HH:mm:ss zzz yyyy")
  private val READ_WORKAROUND_FORMAT = DateTimeFormat.forPattern("MMM dd HH:mm:ss yyyy")

  def parse(s: String): DateTime = {
    val dot = s.lastIndexOf('.')
    val millis = s.substring(dot + 1).toInt
    READ_WORKAROUND_FORMAT.parseDateTime(s.substring(4, dot).replace("EST ", "")).withMillisOfSecond(millis)
  }

  def format(d: DateTime): String = d.toString(FORMAT) + "." + (d.getMillis % 1000)

  implicit class DateTimeExtension(val d: DateTime) extends AnyVal {
    def asJposAt: String = format(d)
  }
}
