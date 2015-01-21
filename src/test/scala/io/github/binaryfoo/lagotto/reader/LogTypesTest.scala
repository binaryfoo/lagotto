package io.github.binaryfoo.lagotto.reader

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.LagoTest

class LogTypesTest extends LagoTest {

  "load()" should "instantiate LogTypes using reflection" in {
    val config = ConfigFactory.load()

    val types = LogTypes.load(config)
    (types("apache") match {
      case RegexParsedLog(pattern, timeFormat) => (pattern, timeFormat)
    }) shouldBe ("""\[(?<timestamp>\d{2}/\w{3}/\d{4} \d{2}:\d{2}:\d{2} \w{3,4})\].* "(?<url>[^"]+)" (?<responseCode>\d{3}) [+-X] [-0-9]+ (?<responseTime>\d+).*""", "dd/MMM/yyyy HH:mm:ss 'AEDT'")

    types("log4j") shouldBe Log4jLog
    types("jpos") shouldBe JposLog
  }

}
