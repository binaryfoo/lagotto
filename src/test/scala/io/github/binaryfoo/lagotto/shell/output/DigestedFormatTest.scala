package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.dictionary.RootDataDictionary
import io.github.binaryfoo.lagotto.{LagoTest, LogEntry}

class DigestedFormatTest extends LagoTest {

  "Format" should "do something with a LogEntry" in {
    val entry = LogEntry.fromString(contentsOf("one-entry.xml"))
    val output = DigestedFormat(RootDataDictionary()).format(entry)
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="receive" lifespan="10005">
                      |  0 (mti): 0800 (Network Management Request)
                      |  7 (Transmission date and time): 1124000003
                      |  11 (System trace audit number): 28928
                      |  24 (Function code): 831""".stripMargin
  }
}
