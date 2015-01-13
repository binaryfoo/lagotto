package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.{LagoTest, JposEntry}

class DigestedFormatTest extends LagoTest {

  val entry = JposEntry.fromString(contentsOf("one-entry.xml"))

  "Format" should "do something with a LogEntry" in {
    val output = DigestedFormat(RootDataDictionary(), NameType.English).format(entry)
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="receive" lifespan="10005">
                      |  0 (mti): 0800 (Network Management Request)
                      |  7 (Transmission date and time): 1124000003
                      |  11 (System trace audit number): 28928
                      |  24 (Function code): 831""".stripMargin
  }

  it should "allow export names" in {
    val output = DigestedFormat(RootDataDictionary(), NameType.Export).format(entry)
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="receive" lifespan="10005">
                      |  0 (mti): 0800 (Network Management Request)
                      |  7 (transmissionDateAndTime): 1124000003
                      |  11 (stan): 28928
                      |  24 (functionCode): 831""".stripMargin
  }

  it should "unzip gzipped fields" in {
    val output = DigestedFormat(RootDataDictionary(configWithTestDictionary), NameType.English).format(JposEntry.fromString(contentsOf("gzip.xml")))
    output should include("hello digest")
  }
}
