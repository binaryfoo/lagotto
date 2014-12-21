package io.github.binaryfoo.lagotto

import org.joda.time.DateTime
import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import org.scalatest.{FlatSpec, Matchers}

class JposTimestampTest extends FlatSpec with Matchers {

  "The creative date format used in jpos logs" should "be parseable" in {
    JposTimestamp.parse("Mon Nov 24 16:59:03 EST 2014.292") shouldEqual new DateTime(2014, 11, 24, 16, 59, 3, 292)
    JposTimestamp.parse("Sun Nov 23 00:59:03 EST 2014.29") shouldEqual new DateTime(2014, 11, 23, 0, 59, 3, 29)
    JposTimestamp.parse("Tue Nov 25 23:00:00 EST 2014.2") shouldEqual new DateTime(2014, 11, 25, 23, 0, 0, 2)
    JposTimestamp.parse("Mon Nov 24 13:10:55 EST 2014") shouldEqual new DateTime(2014, 11, 24, 13, 10, 55, 0)
  }

  it should "be writable" in {
    JposTimestamp.format(new DateTime(2014, 11, 24, 16, 59, 3, 292)) shouldEqual "Mon Nov 24 16:59:03 EST 2014.292"
    JposTimestamp.format(new DateTime(2014, 11, 25, 23, 0, 0, 2)) shouldEqual "Tue Nov 25 23:00:00 EST 2014.2"
  }

  "DateTime implicit" should "format like 'at' attribute" in {
    new DateTime(2014, 11, 25, 23, 0, 0, 2).asJposAt shouldEqual "Tue Nov 25 23:00:00 EST 2014.2"
  }
}
