package io.github.binaryfoo.lagotto.digest

import io.github.binaryfoo.lagotto.{JposTimestamp, LogEntry}
import org.joda.time.{LocalDate, LocalTime}
import org.scalatest.{FlatSpec, Matchers}

class HistogramTest extends FlatSpec with Matchers {

  def testSet = HistogramSet({e: LogEntry => e("time")}, "time",
    Histogram("auths", {_.mti == "0200"}),
    Histogram("reversals", {_.mti == "0400"}))

  private val today = new LocalDate()

  "A histogram set" should "output two data series in time order" in {
    val set = testSet

    set.add(LogEntry("0" -> "0400", "at" -> todayAt(hhmm(18, 10))))
    set.add(LogEntry("0" -> "0200", "at" -> todayAt(hhmm(11, 1))))
    set.add(LogEntry("0" -> "0200", "at" -> todayAt(hhmm(11, 1))))

    set.toXsv(",").mkString("\n") shouldEqual """time,auths,reversals
                                                |11:01:00.000,2,0
                                                |18:10:00.000,0,1""".stripMargin
  }

  it should "ignore filtered entries" in {
    val set = testSet
    set.add(LogEntry("0" -> "0800", "at" -> todayAt(hhmm(18, 10))))

    set.toXsv(",").mkString("\n") shouldEqual "time,auths,reversals"
  }

  def todayAt(time: LocalTime): String = {
    JposTimestamp.format(today.toDateTime(time))
  }
  
  def hhmm(hour: Int, minute: Int) = new LocalTime(hour, minute)
}
