package io.github.binaryfoo.lagotto

import java.io.File

import io.github.binaryfoo.lagotto.LogLike.IterableOfLogLike
import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable

class LogReaderTest extends LagoTest {

  "A log reader" should "read a single entry" in {
    val entries = readEntries("basic.xml").toList
    val head = entries.head
    head.at shouldEqual "Mon Nov 24 00:00:03 EST 2014.292"
    head("7") shouldEqual "1124000003"
  }

  it should "support conversion of pairs to a .csv file" in {
    val entries = readEntries("a-pair.xml")
    val csv = MsgPair.pair(entries).toCsv("time", "mti", "11", "rtt")

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of pairs to a .csv file succinctly" in {
    val entries = readEntries("a-pair.xml")
    val csv = MsgPair.pair(entries).toCsv("time", "mti", "11", "rtt")

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of entries to a .csv file" in {
    val entries = readEntries("a-pair.xml")
    val csv = entries.toCsv("time", "mti", "11")

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,,""".stripMargin
  }

  it should "support conversion of entries to a .csv file succinctly" in {
    val entries = readEntries("a-pair.xml")
    val csv = entries.toCsv("time", "mti", "11")

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,,""".stripMargin
  }

  it should "support conversion of two paired entries to a .csv file" in {
    val entries = readEntries("a-bunch.xml")
    val csv = entries.pair().toCsv("time", "mti", "11", "4", "39", "rtt")

    csv shouldEqual
      """00:00:04.292,0200,2,5000,01,600
        |00:00:03.292,0200,1,10001,00,1700""".stripMargin
  }

  "Reading 2 files" should "read records in order" in {
    val entries = LogReader().read(List("a-bunch.xml", "a-second-bunch.xml").map(f => new File(testFile(f))))
    val csv = entries.toCsv("time", "48.1")
    csv shouldEqual """00:00:03.292,a-bunch.xml #1
                      |00:00:04.292,a-bunch.xml #2
                      |00:00:04.892,a-bunch.xml #3
                      |00:00:04.992,a-bunch.xml #4
                      |00:00:03.292,a-second-bunch.xml #1
                      |00:00:04.100,a-second-bunch.xml #2""".stripMargin
  }

  def readEntries(s: String): Iterator[LogEntry] = {
    LogReader().read(sourceFrom(s))
  }

}
