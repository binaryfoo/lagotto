package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.LogEntry
import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

class JSONOutputTest extends FlatSpec with Matchers {

  "Json output" should "write some json" in {
    val entry = LogEntry("at" -> new DateTime(0).asJposAt, "0" -> "0200")
    JSONOutput.apply(entry).get shouldBe """{"at":"Thu Jan 01 10:00:00 EST 1970.0","0":"0200"}"""
  }
}
