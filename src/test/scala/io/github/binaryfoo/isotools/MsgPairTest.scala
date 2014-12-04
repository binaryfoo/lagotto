package io.github.binaryfoo.isotools

import java.io.File

import org.scalatest.{Matchers, FlatSpec}

class MsgPairTest extends FlatSpec with Matchers {

  "A single request and response pair" should "be matched" in {
    val request = LogEntry("0" -> "0800", "11" -> "1")
    val response = LogEntry("0" -> "0810", "11" -> "000001")

    val pairs: Iterable[MsgPair] = MsgPair.pair(List(request, response))

    pairs should have size 1
    assert(pairs.head.request === request)
    assert(pairs.head.response === response)
  }

  "Log entries that aren't messages" should "be ignored" in {
    val entries = LogReader.read(new File("src/test/resources/basic.xml"))

    MsgPair.pair(entries) should have size 0
  }

  "A pair read from a file" should "have a round trip time" in {
    val pairs = MsgPair.pair(LogReader.read(new File("src/test/resources/a-pair.xml")))

    pairs.head.rtt shouldEqual 808
  }

  it should "provide access to fields and attributes" in {
    val pair = MsgPair.pair(LogReader.read(new File("src/test/resources/a-pair.xml"))).head

    pair("0") shouldEqual "0800"
    pair("48.1") shouldEqual "subfield 48.1"
    pair("req.time") shouldEqual "00:00:03.292"
    pair("request.time") shouldEqual "00:00:03.292"
    pair("response.time") shouldEqual "00:00:04.100"
    pair("rtt") shouldEqual "808"
    pair("mti") shouldEqual "0800"
    pair("timestamp") shouldEqual "2014-11-24 00:00:03.292"
  }
}
