package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.{LagoTest, JposEntry}

class DigestedFormatTest extends LagoTest {

  val entry = JposEntry.fromString(contentsOf("one-entry.xml"))

  "Format" should "do something with a LogEntry" in {
    val output = DigestedFormat(RootDataDictionary(), Some(NameType.English)).format(entry)
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="receive" lifespan="10005">
                      |  0: 0800 (Network Management Request) [mti]
                      |  7: 1124000003 [Transmission date and time]
                      |  11: 28928 [System trace audit number]
                      |  24: 831 [Function code]
                      |</log>
                      |""".stripMargin
  }

  it should "allow no names" in {
    val output = DigestedFormat(RootDataDictionary(), None).format(entry)
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="receive" lifespan="10005">
                      |  0: 0800 (Network Management Request)
                      |  7: 1124000003
                      |  11: 28928
                      |  24: 831
                      |</log>
                      |""".stripMargin
  }

  it should "allow export names" in {
    val output = DigestedFormat(RootDataDictionary(), Some(NameType.Export)).format(entry)
    output shouldBe """<log realm="some.channel/10.0.0.1:4321" at="2014-11-24 00:00:03.292" type="receive" lifespan="10005">
                      |  0: 0800 (Network Management Request) [mti]
                      |  7: 1124000003 [transmissionDateAndTime]
                      |  11: 28928 [stan]
                      |  24: 831 [functionCode]
                      |</log>
                      |""".stripMargin
  }

  it should "unzip gzipped fields" in {
    val output = DigestedFormat(RootDataDictionary(configWithTestDictionary), Some(NameType.English)).format(JposEntry.fromString(contentsOf("gzip.xml")))
    output should include("hello digest")
  }
}
