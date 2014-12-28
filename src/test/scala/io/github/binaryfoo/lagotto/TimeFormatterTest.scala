package io.github.binaryfoo.lagotto

import org.joda.time.{DateTimeZone, DateTime}
import org.scalatest.{Matchers, FlatSpec}

class TimeFormatterTest extends FlatSpec with Matchers {

  "time(millis)" should "print millis since epoch" in {
    val format = TimeFormatter.unapply("time(millis)").get
    format.print(new DateTime(1970, 1, 1, 0, 0, 0, 0, DateTimeZone.UTC)) shouldBe "0"
  }

  "time(millis)" should "parse millis since epoch" in {
    val format = TimeFormatter.unapply("time(millis)").get
    format.parseDateTime("0") shouldBe new DateTime(0)
  }

}
