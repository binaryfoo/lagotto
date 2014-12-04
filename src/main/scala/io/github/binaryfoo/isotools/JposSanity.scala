package io.github.binaryfoo.isotools

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object JposSanity {

  private val TIMESTAMP_FORMAT = DateTimeFormat.forPattern("MMM dd HH:mm:ss yyyy")

  def parseTimestamp(s: String): DateTime = {
    val dot = s.lastIndexOf('.')
    val millis = s.substring(dot + 1).toInt
    TIMESTAMP_FORMAT.parseDateTime(s.substring(4, dot).replace("EST ", "")).withMillisOfSecond(millis)
  }
}
