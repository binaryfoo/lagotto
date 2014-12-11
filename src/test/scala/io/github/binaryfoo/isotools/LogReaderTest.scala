package io.github.binaryfoo.isotools

import java.io.File

import org.scalatest.{Matchers, FlatSpec}
import io.github.binaryfoo.isotools.Csv.IterableOfMapToCsv
import io.github.binaryfoo.isotools.ConvertibleToMap.IterableOfConvertibleToMap
import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable

import scala.io.Source

class LogReaderTest extends FlatSpec with Matchers {

  "A log reader" should "read a single entry" in {
    val entries = LogReader().read(Source.fromFile("src/test/resources/basic.xml"))
    val head = entries.head
    head.at shouldEqual "Mon Nov 24 00:00:03 EST 2014.292"
    head.field("7") shouldEqual "1124000003"
  }

  it should "support conversion of pairs to a .csv file" in {
    val entries = LogReader().read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = MsgPair.pair(entries).map(_.toMap("time", "mti", "11", "rtt")).toCsv

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of pairs to a .csv file succinctly" in {
    val entries = LogReader().read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = MsgPair.pair(entries).toCsv("time", "mti", "11", "rtt")

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of entries to a .csv file" in {
    val entries = LogReader().read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = entries.map(_.toMap("time", "mti", "11")).toCsv

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,,""".stripMargin
  }

  it should "support conversion of entries to a .csv file succinctly" in {
    val entries = LogReader().read(Source.fromFile("src/test/resources/a-pair.xml"))
    val csv = entries.toCsv("time", "mti", "11")

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,,""".stripMargin
  }

  it should "support conversion of two paired entries to a .csv file" in {
    val entries = LogReader().read(Source.fromFile("src/test/resources/a-bunch.xml"))
    val csv = entries.pair().toCsv("time", "mti", "11", "4", "39", "rtt")

    csv shouldEqual
      """00:00:04.292,0200,2,5000,01,600
        |00:00:03.292,0200,1,10001,00,1700""".stripMargin
  }

  "Reading 2 files" should "read records in order" in {
    val entries = LogReader().read(List("src/test/resources/a-bunch.xml", "src/test/resources/a-second-bunch.xml").map(new File(_)))
    val csv = entries.toCsv("time", "48.1")
    csv shouldEqual """00:00:03.292,a-bunch.xml #1
                      |00:00:04.292,a-bunch.xml #2
                      |00:00:04.892,a-bunch.xml #3
                      |00:00:04.992,a-bunch.xml #4
                      |00:00:03.292,a-second-bunch.xml #1
                      |00:00:04.100,a-second-bunch.xml #2""".stripMargin
  }

}
