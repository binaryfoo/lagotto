package io.github.binaryfoo.isotools

import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

class LogReaderTest extends FlatSpec with Matchers {

  "A log reader" should "read a single entry" in {
    val entries: Iterable[LogEntry] = LogReader.read(Source.fromFile("src/test/resources/basic.xml"))
    entries should have size 2
    entries.head.at shouldEqual "Mon Nov 24 00:00:03 EST 2014.292"
    entries.head.field("7") shouldEqual "1124000003"
  }
}
