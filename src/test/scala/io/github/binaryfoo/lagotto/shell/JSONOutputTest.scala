package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import io.github.binaryfoo.lagotto.output.GZip
import io.github.binaryfoo.lagotto.{LagoTest, JposEntry}
import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.shell.output.{DigestedFormat, JSONOutput}
import org.joda.time.{DateTimeZone, DateTime}
import org.scalatest.{FlatSpec, Matchers}

class JSONOutputTest extends LagoTest {

  "JSON output" should "write some json" in {
    val entry = JposEntry("at" -> new DateTime(0L, UYST_TZ).asJposAt, "0" -> "0200", "11" -> "012345", "lifespan" -> "1001", "48.1" -> "private")
    val output = JSONOutput(RootDataDictionary()).apply(entry).get
    output shouldBe """{"at":"1969-12-31T21:00:00.000-0300","mti":"0200","stan":12345,"lifespan":1001,"f_48_1":"private"}"""
  }

  it should "unpack a gzipped string" in {
    val entry = JposEntry("at" -> new DateTime(0L, UYST_TZ).asJposAt, "realm" -> "acme-terminal", "259" -> GZip.zip("line one\nline two"))
    val output = JSONOutput(RootDataDictionary(configWithTestDictionary)).apply(entry).get
    output shouldBe """{"at":"1969-12-31T21:00:00.000-0300","realm":"acme-terminal","aZippedMessage":"line one\nline two"}"""
  }
}
