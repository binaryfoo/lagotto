package io.github.binaryfoo.lagotto.reader

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.{IAmSorryDave, LagoTest}
import io.github.binaryfoo.lagotto.reader.LogTypes.RichLogTypes

class AutoDetectLogTest extends LagoTest {

    private val logTypes = """logTypes: {
                     |  "a": {
                     |    class: "io.github.binaryfoo.lagotto.reader.RegexParsedLog"
                     |    args: [
                     |      "(?<timestamp>\\d{2}/\\d{2}/\\d{4}): (?<message>.+)"
                     |      "dd/MM/yyyy"
                     |      "io.github.binaryfoo.lagotto.reader.AnyLineRecogniser"
                     |    ]
                     |  }
                     |  "b": {
                     |    class: "io.github.binaryfoo.lagotto.reader.RegexParsedLog"
                     |    args: [
                     |      "SILLY (?<timestamp>\\d{2}-\\w{3}-\\d{4}) \\*\\*\\* (?<message>.+)"
                     |      "dd-MMM-yyyy"
                     |      "io.github.binaryfoo.lagotto.reader.AnyLineRecogniser"
                     |    ]
                     |  }
                     |}""".stripMargin
  private val types = LogTypes.load(ConfigFactory.parseString(logTypes).getObject("logTypes"))

  it should "produce a log type that sniffs lines" in {
    val sourceLines = new LineIterator(inputStreamFrom("01/02/1970: Month after Epoch", "SILLY 01-JAN-1971 *** Year after"), "", true, true)
    val log = new AutoDetectLog(types.list(Seq("a", "b")))

    log(sourceLines).toCsv("time(yyyy-MM-dd)", "message") shouldBe "1970-02-01,Month after Epoch"
    log(sourceLines).toCsv("time(yyyy-MM-dd)", "message") shouldBe "1971-01-01,Year after"
  }

  it should "complain if unable to parse a line" in {
    val sourceLines = new LineIterator(inputStreamFrom("01/02/1970: Month after Epoch", "fail"), "theFile", true, true)
    val log = new AutoDetectLog(types.list(Seq("a", "b")))

    log(sourceLines)
    the [IAmSorryDave] thrownBy {
      log(sourceLines)
    } should have message """Can't parse theFile:2 'fail'"""
  }

  "with reference types" should "ignore rubbish before log4j" in {
    val config = ConfigFactory.load()
    val logTypes = LogTypes.load(config)
    val log = LogTypes.auto(config, logTypes)

    val lineIterator = new LineIterator(inputStreamFrom("rubbish", "[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful"), "", true, true)
    the [IAmSorryDave] thrownBy {
      log(lineIterator)
    } should have message "Can't parse :1 'rubbish'"
    log(lineIterator).lines shouldBe "[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful"
  }
}
