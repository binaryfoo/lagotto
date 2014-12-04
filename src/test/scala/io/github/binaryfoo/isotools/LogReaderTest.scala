package io.github.binaryfoo.isotools

import org.scalatest.{Matchers, FlatSpec}
import io.github.binaryfoo.isotools.Csv.IterableOfMapToCsv
import io.github.binaryfoo.isotools.Csv.IterableOfConvertibleToMap

import scala.io.Source

class LogReaderTest extends FlatSpec with Matchers {

  "A log reader" should "read a single entry" in {
    val entries = LogReader.read(Source.fromFile("src/test/resources/basic.xml"))
    entries should have size 2
    entries.head.at shouldEqual "Mon Nov 24 00:00:03 EST 2014.292"
    entries.head.field("7") shouldEqual "1124000003"
  }

  it should "support conversion of pairs to a .csv file" in {
    val entries = LogReader.read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = MsgPair.pair(entries).map(_.toMap("time", "mti", "11", "rtt")).toCsv

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of pairs to a .csv file succinctly" in {
    val entries = LogReader.read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = MsgPair.pair(entries).toCsv("time", "mti", "11", "rtt")

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of entries to a .csv file" in {
    val entries = LogReader.read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = entries.map(_.toMap("time", "mti", "11")).toCsv

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,null,null""".stripMargin
  }

  it should "support conversion of entries to a .csv file succinctly" in {
    val entries = LogReader.read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = entries.toCsv("time", "mti", "11")

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,null,null""".stripMargin
  }


}
