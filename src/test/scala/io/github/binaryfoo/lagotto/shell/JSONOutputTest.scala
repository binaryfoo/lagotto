package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import io.github.binaryfoo.lagotto.LogEntry
import io.github.binaryfoo.lagotto.dictionary.RootDataDictionary
import io.github.binaryfoo.lagotto.shell.output.JSONOutput
import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

class JSONOutputTest extends FlatSpec with Matchers {

  "JSON output" should "write some json" in {
    val entry = LogEntry("at" -> new DateTime(0).asJposAt, "0" -> "0200", "11" -> "012345", "lifespan" -> "1001", "48.1" -> "private")
    val output = JSONOutput(RootDataDictionary()).apply(entry).get
    output shouldBe """{"at":"1970-01-01T10:00:00.000+1000","mti":"0200","stan":12345,"lifespan":1001,"48.1":"private"}"""
  }
}
