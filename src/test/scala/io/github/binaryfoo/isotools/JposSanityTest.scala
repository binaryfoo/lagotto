package io.github.binaryfoo.isotools

import io.github.binaryfoo.isotools.JposSanity.parseTimestamp
import org.joda.time.DateTime
import org.scalatest.{Matchers, FlatSpec}

class JposSanityTest extends FlatSpec with Matchers {

  "The creative date format used in jpos logs" should "be parseable" in {
    parseTimestamp("Mon Nov 24 16:59:03 EST 2014.292") shouldEqual new DateTime(2014, 11, 24, 16, 59, 3, 292)
    parseTimestamp("Sun Nov 23 00:59:03 EST 2014.29") shouldEqual new DateTime(2014, 11, 23, 0, 59, 3, 29)
    parseTimestamp("Tue Nov 25 23:00:00 EST 2014.2") shouldEqual new DateTime(2014, 11, 25, 23, 0, 0, 2)
  }
}
