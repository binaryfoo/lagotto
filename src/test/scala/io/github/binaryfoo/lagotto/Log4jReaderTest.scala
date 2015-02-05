package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.reader.{LogReader, Log4jLog}

import scala.io.Source

class Log4jReaderTest extends LagoTest {

  private val twoLines = """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful
    |[08 Nov 2014 00:00:00,002] INFO  [a.ClassName]: And again""".stripMargin
  
  "Log4j reader" should "read two records" in {
    val entries = LogReader(logType = Log4jLog).read(inputStreamFrom(twoLines))
    entries.next().message shouldBe "Did something useful"
    entries.next().message shouldBe "And again"
    entries.hasNext shouldBe false
  }

  it should "record the sourceName and line number" in {
    val entries = LogReader(logType = Log4jLog).read(inputStreamFrom(twoLines), "name.log")
    entries.next().source shouldBe new SourceRef("name.log", 1)
    entries.next().source shouldBe new SourceRef("name.log", 2)
  }

  it should "read a multiline message" in {
    val input =
      """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful
        |And then some more
        |Last line""".stripMargin

    val entries = LogReader(logType = Log4jLog).read(inputStreamFrom(input))
    entries.next().message shouldBe """Did something useful
                                      |And then some more
                                      |Last line""".stripMargin
  }

}
