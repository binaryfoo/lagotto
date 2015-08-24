package io.github.binaryfoo.lagotto

import java.lang.Integer.parseInt
import java.text.SimpleDateFormat

import org.joda.time.format.DateTimeFormat
import org.joda.time.{DateTime, DateTimeZone}

import scala.collection.mutable

object JposTimestamp {

  private val FORMAT = DateTimeFormat.forPattern("EEE MMM dd HH:mm:ss zzz yyyy")

  def parse(s: String): DateTime = {
    try {
      // Nuanced:
      // 1. Day names don't seem to parse so well
      // 2. Optional milliseconds are also troublesome
      // 3. The timezone id (zzz) needs to be read the java.util way
      // 4. Joda's new DateTime(java.util.Date) leaves us with an instance that has GJChronology instead of
      //    an ISOChronology which breaks equals()

      val start = 4
      val month = s.substring(start, start + 3)
      val day = parseInt(s.substring(start + 4, start + 4 + 2))
      val hour = parseInt(s.substring(start + 7, start + 7 + 2))
      val minute = parseInt(s.substring(start + 10, start + 10 + 2))
      val second = parseInt(s.substring(start + 13, start + 13 + 2))

      // Milliseconds moved with https://github.com/jpos/jPOS/commit/980c1a4
      if (s(start + 15) == '.') {
        val endOfMillis = s.indexOf(' ', start + 16)
        val millis = parseInt(s.substring(start + 16, endOfMillis))
        val endOfZone = s.indexOf(' ', endOfMillis + 1)
        val zone = s.substring(endOfMillis + 1, endOfZone)
        val year = parseInt(s.substring(endOfZone + 1, endOfZone + 5))

        new DateTime(year, EnglishMonths.months(month), day, hour, minute, second, millis, juTimeZone(zone))
      } else {
        val endOfZone = s.indexOf(' ', start + 16)
        val zone = s.substring(start + 16, endOfZone)
        val year = parseInt(s.substring(endOfZone + 1, endOfZone + 5))
        val dot = s.lastIndexOf('.', endOfZone + 5)
        val millis = if (dot == -1) 0 else parseInt(s.substring(dot + 1))

        new DateTime(year, EnglishMonths.months(month), day, hour, minute, second, millis, juTimeZone(zone))
      }
    }
    catch {
      case e: Throwable => throw new IllegalArgumentException(s"Failed to parse time: $s", e)
    }
  }

  private val tzCache = new mutable.HashMap[String, DateTimeZone]()

  private def juTimeZone(javaUtilId: String): DateTimeZone = {
    tzCache.getOrElseUpdate(javaUtilId, {
      val format = new SimpleDateFormat("zzz")
      format.parse(javaUtilId)
      val zone = DateTimeZone.forTimeZone(format.getCalendar.getTimeZone)
      tzCache.put(javaUtilId, zone)
      zone
    })
  }

  def format(d: DateTime): String = d.toString(FORMAT) + "." + d.getMillisOfSecond

  implicit class DateTimeExtension(val d: DateTime) extends AnyVal {
    def asJposAt: String = format(d)
  }
}

object EnglishMonths {
  val months = Map("Jan" -> 1, "Feb" -> 2, "Mar" -> 3, "Apr" -> 4, "May" -> 5, "Jun" -> 6, "Jul" -> 7, "Aug" -> 8, "Sep" -> 9, "Oct" -> 10, "Nov" -> 11, "Dec" -> 12)
}
