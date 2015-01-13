package io.github.binaryfoo.lagotto

import java.io.File

import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import io.github.binaryfoo.lagotto.reader.LogReader
import org.joda.time.DateTime

class MsgPairTest extends LagoTest {

  "A single request and response pair" should "be matched" in {
    val request = JposEntry("0" -> "0800", "11" -> "1")
    val response = JposEntry("0" -> "0810", "11" -> "000001")

    val pairs = MsgPair.pair(iteratorOver(request, response)).toStream

    pairs should have size 1
    assert(pairs.head.request === request)
    assert(pairs.head.response === response)
  }

  it should "pair when field 11 is not numeric" in {
    val request = JposEntry("0" -> "0800", "11" -> "abc")
    val response = JposEntry("0" -> "0810", "11" -> "abc")

    val pairs = MsgPair.pair(iteratorOver(request, response)).toStream

    pairs shouldEqual List(MsgPair(request, response))
  }

  it should "pair response with most recent request in face of duplicates" in {
    val request = JposEntry("0" -> "0800", "11" -> "1", "id" -> "1")
    val dupeRequest = JposEntry("0" -> "0800", "11" -> "1", "id" -> "2")
    val response = JposEntry("0" -> "0810", "11" -> "1", "id" -> "3")

    val pairs = MsgPair.pair(iteratorOver(request, dupeRequest, response)).toStream

    pairs shouldEqual List(MsgPair(dupeRequest, response))
  }

  it should "pairing should use realm" in {
    val request = JposEntry("0" -> "0800", "11" -> "1", "realm" -> "a")
    val wrongResponse = JposEntry("0" -> "0810", "11" -> "1", "realm" -> "b")
    val response = JposEntry("0" -> "0810", "11" -> "1", "realm" -> "a")

    val pairs = MsgPair.pair(iteratorOver(request, wrongResponse, response)).toStream

    pairs shouldEqual List(MsgPair(request, response))
  }

  "Log entries that aren't messages" should "be ignored" in {
    val entries = LogReader().read(new File(testFile("basic.xml")))

    MsgPair.pair(entries) should have size 0
  }

  "A pair read from a file" should "have a round trip time" in {
    val pairs = MsgPair.pair(LogReader().read(new File(testFile("a-pair.xml")))).toStream

    pairs.head.rtt shouldEqual 808
  }

  it should "provide access to fields and attributes" in {
    val pair = MsgPair.pair(LogReader().read(new File(testFile("a-pair.xml")))).next()

    pair("0") shouldEqual "0800"
    pair("48.1") shouldEqual "subfield 48.1"
    pair("req.time") shouldEqual "00:00:03.292"
    pair("request.time") shouldEqual "00:00:03.292"
    pair("response.time") shouldEqual "00:00:04.100"
    pair("rtt") shouldEqual "808"
    pair("mti") shouldEqual "0800"
    pair("timestamp") shouldEqual "2014-11-24 00:00:03.292"
  }

  def pair(requestFields: (String, String)*): MsgPair = MsgPair(JposEntry(requestFields : _*), JposEntry())

  "A sequence of msgs" should "be coalesceable by mti" in {
    val now = new DateTime()

    val auth1 = pair("at" -> now.asJposAt, "0" -> "0200")
    val auth2 = pair("at" -> now.plusMillis(100).asJposAt, "0" -> "0200")
    val auth3 = pair("at" -> now.plusMillis(200).asJposAt, "0" -> "0200")
    val key1 = pair("at" -> now.plusMillis(300).asJposAt, "0" -> "0820")
    val auth4 = pair("at" -> now.plusMillis(400).asJposAt, "0" -> "0200")

    val seq = iteratorOver(auth1, auth2, auth3, key1, auth4)
    val coalesced = MsgPair.coalesce(seq, _.mti).toStream

    coalesced shouldEqual List(auth1, Group(2, "0200"), key1, auth4)
  }

  it should "not coalesce on 53 when 53 changes for each message" in {
    val one = pair("53" -> "1")
    val two = pair("53" -> "2")

    MsgPair.coalesce(iteratorOver(one, two), _("53")).toStream shouldEqual List(one, two)
  }

  it should "coalesce two messages with same value in 53" in {
    val one = pair("53" -> "1")
    val two = pair("53" -> "1")

    MsgPair.coalesce(iteratorOver(one, two), _("53")).toStream shouldEqual List(one, Group(1, "1"))
  }

  "A single pair" should "be reduceable to a map" in {
    val p = MsgPair(JposEntry("11" -> "123456", "37" -> "ignored"), JposEntry("39" -> "00"))

    p.toSeq("11", "39") shouldEqual Seq("123456", "00")
  }
}
