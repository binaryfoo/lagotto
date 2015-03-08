package io.github.binaryfoo.lagotto

import java.io.{FileOutputStream, File}
import java.nio.file.Files

import io.github.binaryfoo.lagotto.LogEntry.IterableOfLogEntry
import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable
import io.github.binaryfoo.lagotto.reader.{SingleThreadLogReader, LogReader}

class LogReaderTest extends LagoTest {
  import fieldParser.stringAsFieldExpr

  "A log reader" should "read a single entry" in {
    val entries = readEntries("basic.xml").toList
    val head = entries.head
    head.at shouldEqual "Mon Nov 24 00:00:03 EST 2014.292"
    head("7") shouldEqual "1124000003"
  }

  it should "support conversion of pairs to a .csv file" in {
    val entries = readEntries("a-pair.xml")
    val csv = MsgPair.pair(entries).exprToCsv("time", "mti", "11", "rtt")

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of pairs to a .csv file succinctly" in {
    val entries = readEntries("a-pair.xml")
    val csv = MsgPair.pair(entries).exprToCsv("time", "mti", "11", "rtt")

    csv shouldEqual "00:00:03.292,0800,28928,808"
  }

  it should "support conversion of entries to a .csv file" in {
    val entries = readEntries("a-pair.xml")
    val csv = entries.exprToCsv("time", "mti", "11")

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,,""".stripMargin
  }

  it should "support conversion of entries to a .csv file succinctly" in {
    val entries = readEntries("a-pair.xml")
    val csv = entries.exprToCsv("time", "mti", "11")

    csv shouldEqual
      """00:00:03.292,0800,28928
        |00:00:04.100,0810,28928
        |13:10:55.000,,""".stripMargin
  }

  it should "support conversion of two paired entries to a .csv file" in {
    val entries = readEntries("a-bunch.xml")
    val csv = entries.pair().exprToCsv("time", "mti", "11", "4", "39", "rtt")

    csv shouldEqual
      """00:00:04.292,0200,2,5000,01,600
        |00:00:03.292,0200,1,10001,00,1700""".stripMargin
  }

  it should "tag each record with a line starting line number" in {
    val parser = new FieldExprParser()
    import parser.stringAsFieldExpr

    val entries = readEntries("a-bunch.xml")
    val csv = entries.exprToCsv("48.1", "src")
    csv shouldEqual """a-bunch.xml #1,:3
                      |a-bunch.xml #2,:15
                      |a-bunch.xml #3,:27
                      |a-bunch.xml #4,:42""".stripMargin
  }

  "Reading 2 files" should "read records in order" in {
    val entries = LogReader().read(testFiles("a-bunch.xml", "a-second-bunch.xml"), follow = false)
    val csv = entries.exprToCsv("time", "48.1")
    csv shouldEqual """00:00:03.292,a-bunch.xml #1
                      |00:00:04.292,a-bunch.xml #2
                      |00:00:04.892,a-bunch.xml #3
                      |00:00:04.992,a-bunch.xml #4
                      |00:00:03.292,a-second-bunch.xml #1
                      |00:00:04.100,a-second-bunch.xml #2""".stripMargin
  }

  "A single thread log reader" should "read a single entry" in {
    val entries = SingleThreadLogReader().read(sourceFrom("basic.xml")).toList
    val head = entries.head
    head.at shouldEqual "Mon Nov 24 00:00:03 EST 2014.292"
    head("7") shouldEqual "1124000003"
  }

  it should "tag each record with a line starting line number" in {
    val parser = new FieldExprParser()
    import parser.stringAsFieldExpr

    val entries = SingleThreadLogReader().read(new File(testFile("a-bunch.xml")))
    val csv = entries.exprToCsv("48.1", "src")
    csv shouldEqual """a-bunch.xml #1,a-bunch.xml:3
                      |a-bunch.xml #2,a-bunch.xml:15
                      |a-bunch.xml #3,a-bunch.xml:27
                      |a-bunch.xml #4,a-bunch.xml:42""".stripMargin
  }

  "Follow mode" should "return first entry before blocking" in {
    val file = tempFile()
    copyTestFileTo("one.xml", file)
    val entries: Iterator[JposEntry] = LogReader().read(List(file), follow = true)
    entries.next().lines shouldBe contentsOf("one.xml").trim
    afterDelay(50, copyTestFileTo("two.xml", file))
    entries.next().lines shouldBe contentsOf("two.xml").trim
  }

  private def copyTestFileTo(src: String, dest: File) = {
    val out = new FileOutputStream(dest, true)
    Files.copy(new File(testFile(src)).toPath, out)
    out.close()
  }

  private def readEntries(s: String): Iterator[JposEntry] = {
    LogReader().read(sourceFrom(s))
  }

}
