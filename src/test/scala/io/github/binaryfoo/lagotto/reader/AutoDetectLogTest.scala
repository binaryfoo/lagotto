package io.github.binaryfoo.lagotto.reader

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.LagoTest
import io.github.binaryfoo.lagotto.reader.LogTypes.RichLogTypes

import scala.collection.JavaConversions.asScalaBuffer

class AutoDetectLogTest extends LagoTest {

  it should "produce a log type that sniffs lines" in {
    val logTypes = """logTypes: [
                     |  {
                     |    name: "a"
                     |    class: "io.github.binaryfoo.lagotto.reader.RegexParsedLog"
                     |    args: [
                     |    "(?<timestamp>\\d{2}/\\d{2}/\\d{4}): (?<message>.+)"
                     |    "dd/MM/yyyy"
                     |    ]
                     |  }
                     |  {
                     |    name: "b"
                     |    class: "io.github.binaryfoo.lagotto.reader.RegexParsedLog"
                     |    args: [
                     |    "SILLY (?<timestamp>\\d{2}-\\w{3}-\\d{4}) \\*\\*\\* (?<message>.+)"
                     |    "dd-MMM-yyyy"
                     |    ]
                     |  }
                     |]""".stripMargin
    val sourceLines = new SourceLineIterator(iteratorOver("01/02/1970: Month after Epoch", "SILLY 01-JAN-1971 *** Year after"), "", true, true)

    val types = LogTypes.load(ConfigFactory.parseString(logTypes).getList("logTypes"))
    val log = new AutoDetectLog(types.list(Seq("a", "b")))

    log(sourceLines).toCsv("time(yyyy-MM-dd)", "message") shouldBe "1970-02-01,Month after Epoch"
    log(sourceLines).toCsv("time(yyyy-MM-dd)", "message") shouldBe "1971-01-01,Year after"
  }

}
