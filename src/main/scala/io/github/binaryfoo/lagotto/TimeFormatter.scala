package io.github.binaryfoo.lagotto

import org.joda.time.format.{DateTimeFormat, DateTimeFormatter, PeriodFormatter, PeriodFormatterBuilder}
import org.joda.time.{DateTime, Period}

trait TimeFormatter {
  def print(timestamp: DateTime): String
  def print(period: Period): String
  def parseDateTime(s: String): DateTime
}

class HumanTimeFormatter(val pattern: String) extends TimeFormatter {
  type TimestampPrinter = (DateTime, DateTimeFormatter) => String

  val (jodaPattern, printer): (String, TimestampPrinter) = pattern match {
    case "HH:mm:s0" => ("HH:mm:ss", (timestamp, f) => f.print(timestamp).substring(0, 7) + "0")
    case "HH:m0"    => ("HH:mm",    (timestamp, f) => f.print(timestamp).substring(0, 4) + "0")
    case _          => (pattern,    (timestamp, f) => f.print(timestamp))
  }
  val jodaFormatter = DateTimeFormat.forPattern(jodaPattern)
  private lazy val periodFormatter = PeriodFormatTranslator.translate(jodaPattern)

  override def print(timestamp: DateTime): String = printer(timestamp, jodaFormatter)
  override def print(period: Period): String = periodFormatter.print(period)
  override def parseDateTime(s: String) = jodaFormatter.parseDateTime(s)
}

object EpochTimeFormatter extends TimeFormatter {
  override def print(timestamp: DateTime): String = timestamp.getMillis.toString
  override def print(period: Period): String = new DateTime(0).withPeriodAdded(period, 1).getMillis.toString
  override def parseDateTime(s: String): DateTime = new DateTime(s.toLong)
}

object EpochSecondsFormatter extends TimeFormatter {
  override def print(timestamp: DateTime): String = (timestamp.getMillis/1000).toString
  override def print(period: Period): String = (new DateTime(0).withPeriodAdded(period, 1).getMillis/1000).toString
  override def parseDateTime(s: String): DateTime = new DateTime(s.toLong*1000)
}

object DefaultDateTimeFormat extends HumanTimeFormatter("yyyy-MM-dd HH:mm:ss.SSS")
object ISO8601TimeFormat extends HumanTimeFormatter("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
object DefaultTimeFormat extends HumanTimeFormatter("HH:mm:ss.SSS")
object DefaultDateFormat extends HumanTimeFormatter("yyyy-MM-dd")

object TimeFormatter {

  private val TimeExpression = """time\((.*)\)""".r

  def unapply(expr: FieldExpr): Option[TimeFormatter] = expr.field match {
    case AggregateOps.OverExpression(TimeFormatter(format)) => Some(format)
    case TimeFormatter(format) => Some(format)
    case _ => None
  }

  def unapply(expr: String): Option[TimeFormatter] = expr match {
    case "time" => Some(DefaultTimeFormat)
    case "timestamp" => Some(DefaultDateTimeFormat)
    case "date" => Some(DefaultDateFormat)
    case "time(ms)" => Some(EpochTimeFormatter)
    case "time(millis)" => Some(EpochTimeFormatter)
    case "time(s)" => Some(EpochSecondsFormatter)
    case "time(seconds)" => Some(EpochSecondsFormatter)
    case TimeExpression(pattern) => Some(new HumanTimeFormatter(pattern))
    case MsgPairFieldAccess(_, TimeFormatter(formatter)) => Some(formatter)
    case _ => None
  }

  /**
   * Unapply or die.
   */
  def formatterFor(expr: String) = unapply(expr).get
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