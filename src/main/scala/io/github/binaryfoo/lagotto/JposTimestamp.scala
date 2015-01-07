package io.github.binaryfoo.lagotto

import java.text.SimpleDateFormat

import org.joda.time.{DateTimeZone, DateTime}
import org.joda.time.format.DateTimeFormat

object JposTimestamp {

  private val FORMAT = DateTimeFormat.forPattern("EEE MMM dd HH:mm:ss zzz yyyy")
  private val READ_FORMAT = "MMM dd HH:mm:ss zzz yyyy"

  def parse(s: String): DateTime = {
    try {
      // Nuanced:
      // 1. Day names don't seem to parse so well
      // 2. Optional milliseconds are also troublesome
      // 3. The timezone id (zzz) needs to be read the java.util way
      // 4. Joda's new DateTime(java.util.Date) leaves us with an instance that has GJChronology instead of
      //    an ISOChronology which breaks equals()

      val dot = s.lastIndexOf('.')
      val millis = if (dot == -1) 0 else s.substring(dot + 1).toInt
      val end = if (dot == -1) s.length else dot
      val withoutDayNameAndMillis = s.substring(4, end)

      val juFormat = new SimpleDateFormat(READ_FORMAT)
      val date = juFormat.parse(withoutDayNameAndMillis)
      val timeZone = DateTimeZone.forTimeZone(juFormat.getCalendar.getTimeZone)
      new DateTime(date.getTime, timeZone).withMillisOfSecond(millis)
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
