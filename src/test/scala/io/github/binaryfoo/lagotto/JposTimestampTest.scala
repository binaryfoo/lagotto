package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import org.joda.time.DateTime
import org.joda.time.DateTimeZone.UTC

class JposTimestampTest extends LagoTest {

  "The creative date format used in jpos logs" should "be parseable" in {
    JposTimestamp.parse("Mon Nov 24 16:59:03 EST 2014.292") shouldEqual new DateTime(2014, 11, 24, 16, 59, 3, 292, EST_TZ)
    JposTimestamp.parse("Sun Nov 23 00:59:03 EST 2014.29") shouldEqual new DateTime(2014, 11, 23, 0, 59, 3, 29, EST_TZ)
    JposTimestamp.parse("Tue Nov 25 23:00:00 EST 2014.2") shouldEqual new DateTime(2014, 11, 25, 23, 0, 0, 2, EST_TZ)
    JposTimestamp.parse("Mon Nov 24 13:10:55 EST 2014") shouldEqual new DateTime(2014, 11, 24, 13, 10, 55, 0, EST_TZ)
  }

  it should "be writable" in {
    JposTimestamp.format(new DateTime(2014, 11, 24, 16, 59, 3, 292, UTC)) shouldEqual s"Mon Nov 24 16:59:03 UTC 2014.292"
    JposTimestamp.format(new DateTime(2014, 11, 25, 23, 0, 0, 2, UTC)) shouldEqual s"Tue Nov 25 23:00:00 UTC 2014.2"
  }

  "DateTime implicit" should "format like 'at' attribute" in {
    new DateTime(2014, 11, 25, 23, 0, 0, 2, UTC).asJposAt shouldEqual s"Tue Nov 25 23:00:00 UTC 2014.2"
  }

  "Parse" should "make a token effort with timezones" in {
    JposTimestamp.parse("Thu Dec 18 15:12:40 UYST 2014") shouldEqual new DateTime(2014, 12, 18, 15, 12, 40, 0, UYST_TZ)
  }
}
