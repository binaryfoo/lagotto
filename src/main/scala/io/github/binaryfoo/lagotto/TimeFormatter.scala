package io.github.binaryfoo.lagotto

import org.joda.time.format.{DateTimeFormat, DateTimeFormatter, PeriodFormatter, PeriodFormatterBuilder}
import org.joda.time.{DateTime, Period}

class TimeFormatter(val pattern: String) {
  type TimestampPrinter = (DateTime, DateTimeFormatter) => String

  val (jodaPattern, printer): (String, TimestampPrinter) = pattern match {
    case "HH:mm:s0" => ("HH:mm:ss", (timestamp, f) => f.print(timestamp).substring(0, 7) + "0")
    case "HH:m0"    => ("HH:mm",    (timestamp, f) => f.print(timestamp).substring(0, 4) + "0")
    case _          => (pattern,    (timestamp, f) => f.print(timestamp))
  }
  val jodaFormatter = DateTimeFormat.forPattern(jodaPattern)
  private lazy val periodFormatter = PeriodFormatTranslator.translate(jodaPattern)

  def print(timestamp: DateTime): String = printer(timestamp, jodaFormatter)
  def print(period: Period): String = periodFormatter.print(period)
  def parseLocalTime(s: String) = jodaFormatter.parseLocalTime(s)
  def parseDateTime(s: String) = jodaFormatter.parseDateTime(s)
}

object DefaultDateTimeFormat extends TimeFormatter("yyyy-MM-dd HH:mm:ss.SSS")
object DefaultTimeFormat extends TimeFormatter("HH:mm:ss.SSS")
object DefaultDateFormat extends TimeFormatter("yyyy-MM-dd")

object TimeFormatter {

  private val TimeExpression = """time\((.*)\)""".r

  def unapply(expr: String): Option[TimeFormatter] = expr match {
    case "time" => Some(DefaultTimeFormat)
    case "timestamp" => Some(DefaultDateTimeFormat)
    case "date" => Some(DefaultDateFormat)
    case TimeExpression(pattern) => Some(new TimeFormatter(pattern))
    case _ => None
  }
}

object PeriodFormatTranslator {
  def translate(pattern: String): PeriodFormatter = {
    val b = new PeriodFormatterBuilder()

    val hours = pattern.contains("HH")
    val minutes = pattern.contains("mm")
    val seconds = pattern.contains("ss")
    val millis = pattern.contains("SSS")

    if (pattern.contains("yyyy")) {
      b.appendYears()
      b.appendSuffix(" year ", " years ")
    }

    if (pattern.contains("MM")) {
      b.appendMonths()
      b.appendSuffix(" month ", " months ")
    }

    if (pattern.contains("dd")) {
      b.appendDays()
      val delimiter = if (hours || minutes || seconds || millis) " " else ""
      b.appendSuffix(s" day$delimiter", s" days$delimiter")
    }

    b.printZeroAlways()
    b.minimumPrintedDigits(2)
    var delimit = false

    if (hours) {
      b.appendHours()
      delimit = true
    }

    if (minutes) {
      if (delimit) b.appendLiteral(":")
      b.appendMinutes()
      delimit = true
    }

    if (seconds) {
      if (delimit) b.appendLiteral(":")
      b.appendSeconds()
      delimit = true
    }

    if (millis) {
      if (delimit) b.appendLiteral(".")
      b.appendMillis3Digit()
    }

    b.toFormatter
  }
}