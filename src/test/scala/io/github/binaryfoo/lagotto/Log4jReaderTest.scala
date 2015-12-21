package io.github.binaryfoo.lagotto

import java.io.File

import io.github.binaryfoo.lagotto.reader.{Log4jLog, LogReader}

class Log4jReaderTest extends LagoTest {

  private val twoLines = """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful
    |[08 Nov 2014 00:00:00,002] INFO  [a.ClassName]: And again""".stripMargin
  
  "Log4j reader" should "read two records" in {
    val entries = LogReader(logType = Log4jLog).read(inputStreamFrom(twoLines))
    val first = entries.next()
    val second = entries.next()
    first.message shouldBe "Did something useful"
    first.source.line shouldBe 1
    second.message shouldBe "And again"
    second.source.line shouldBe 2
    entries.hasNext shouldBe false
  }

  it should "record the sourceName and line number" in {
    val file = new File("name.log")
    val entries = LogReader(logType = Log4jLog).read(inputStreamFrom(twoLines), FileRef(file))
    entries.next().source shouldBe FileRef(file, 1)
    entries.next().source shouldBe FileRef(file, 2)
  }

  it should "read a multiline message" in {
    val input =
      """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful
        |And then some more
        |Last line""".stripMargin

    val entries = LogReader(logType = Log4jLog).read(inputStreamFrom(input))
    entries.next().payload shouldBe """Did something useful
                                      |And then some more
                                      |Last line""".stripMargin
  }

}
