package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.{LogEntry, LagoTest}

class DigestedFormatTest extends LagoTest {

  "Format" should "do something with a LogEntry" in {
    val entry = LogEntry.fromString(contentsOf("one-entry.xml"))
    DigestedFormat.format(entry) shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="receive" lifespan="10005">
                                            |  0 (mti): 0800
                                            |  7 (Transmission date and time): 1124000003
                                            |  11 (System trace audit number): 28928
                                            |  24 (Function code): 831""".stripMargin
  }
}
