package io.github.binaryfoo.lagotto

import org.joda.time.DateTime
import io.github.binaryfoo.lagotto.output.Xsv.SeqToXsv

class JposEntryTest extends LagoTest {

  import fieldParser.stringAsFieldAccessor

  val timestamp = "Mon Nov 24 16:59:03 EST 2014.292"
  val realm = "some.channel/10.0.0.1:4321"
  val lifespan = """10005ms"""
  val lines = oneEntry()

  "Log entry parser" should "parse extract fields" in {
    val entry = JposEntry.fromLines(lines)
    entry("7") shouldBe "1124000003"
    entry("48.2.13") shouldBe "subfield 48.2.13"
  }

  it should "extract realm" in {
    val entry = JposEntry.fromLines(lines)
    entry.realm.raw shouldEqual realm
  }

  it should "extract 'at' attribute" in {
    val entry = JposEntry.fromLines(lines)
    entry.at shouldEqual timestamp
  }

  it should "parse the 'at' attribute as a timestamp" in {
    val entry = JposEntry.fromLines(lines)
    entry.timestamp shouldEqual new DateTime(2014, 11, 24, 16, 59, 3, 292, EST_TZ)
  }

  it should "parse the 'lifespan' attribute as a number" in {
    val entry = JposEntry.fromLines(lines)
    entry.lifespan shouldEqual Some(10005)
  }

  it should "handle a missing 'lifespan' attribute" in {
    val entry = JposEntry.fromString(s"""<log realm="$realm" at="$timestamp">
                                        |</log>""".stripMargin)
    entry.lifespan shouldEqual None
  }

  it should "expose both fields and attributes via apply" in {
    import fieldParser.FieldExpr.expressionFor
    val entry = JposEntry.fromLines(lines)

    entry("0") shouldEqual "0800"
    entry("48.2.13") shouldEqual "subfield 48.2.13"
    entry("mti") shouldEqual "0800"
    entry("at") shouldEqual timestamp
    entry("realm") shouldEqual realm
    expressionFor("timestamp")(entry) shouldEqual "2014-11-24 16:59:03.292"
    expressionFor("time")(entry) shouldEqual "16:59:03.292"
    expressionFor("date")(entry) shouldEqual "2014-11-24"
    expressionFor("time(HH:mm)")(entry) shouldEqual "16:59"
    expressionFor("time(HH:mm:s0)")(entry) shouldEqual "16:59:00"
    expressionFor("time(HH:m0)")(entry) shouldEqual "16:50"
    entry("42") shouldEqual null
    entry("rubbish") shouldEqual null
    entry.get("rubbish") shouldEqual None
  }

  it should "extract logical link name from realm" in {
    val entry = JposEntry.fromLines(oneEntry(realm = "linkName.channel/10.0.0.1:4321"))
    entry("link") shouldEqual "linkName"
  }

  it should "extract logical link name from realm without a dot" in {
    val entry = JposEntry.fromLines(oneEntry(realm = "linkName/10.0.0.1:4321"))
    entry("link") shouldEqual "linkName"
  }

  it should "allow arbitrary regex replacement in attributes" in {
    val entry = JposEntry.fromLines(oneEntry(realm = "linkLink.channel/10.0.0.1:4321"))
    entry.exprToSeq("link(/Link//)") shouldEqual Seq("link")
  }

  it should "allow back references in regex replacement in attributes" in {
    val entry = JposEntry.fromLines(oneEntry(realm = "link-1-useless.channel/10.0.0.1:4321"))
    entry.exprToSeq("link(/.*-(\\d)-.*/no-$1/)") shouldEqual Seq("no-1")
  }

  it should "extract logical ip address from realm" in {
    val entry = JposEntry.fromLines(oneEntry(realm = "linkName.channel/10.0.0.1:4321"))
    entry("ipAddress") shouldEqual "10.0.0.1"
    entry("port") shouldEqual "4321"
    entry("socket") shouldEqual "10.0.0.1:4321"
  }

  it should "return empty when realm contains no port" in {
    val entry = JposEntry.fromLines(oneEntry(realm = "linkName.channel/10.0.0.1"))
    entry("ipAddress") shouldEqual "10.0.0.1"
    entry("port") shouldEqual ""
    entry("socket") shouldEqual "10.0.0.1"
  }

  it should "return empty when realm contains no socket" in {
    val entry = JposEntry.fromLines(oneEntry(realm = "linkName.channel"))
    entry("ipAddress") shouldEqual ""
    entry("port") shouldEqual ""
    entry("socket") shouldEqual ""
  }

  it should "extract type" in {
    JposEntry.fromLines(oneEntry(msgType = "receive")).msgType shouldEqual "receive"
    JposEntry.fromLines(oneEntry(msgType = "send")).msgType shouldEqual "send"
  }

  it should "report a <warn> in msgType" in {
    val entry = JposEntry.fromLines(linesFrom("pool-exhaustion.xml"))
    entry("msgType") shouldEqual "warn"
  }

  "io-timeout" should "have msgType io-timeout" in {
    JposEntry.fromLines(linesFrom("io-timeout.xml"))("msgType") shouldBe "io-timeout"
  }

  "peer-disconnect" should "have msgType io-timeout" in {
    JposEntry.fromLines(linesFrom("peer-disconnect.xml"))("msgType") shouldBe "peer-disconnect"
  }

  "connection reset" should "have msgType peer-disconnect" in {
    JposEntry.fromLines(linesFrom("connection-reset.xml"))("msgType") shouldBe "peer-disconnect"
  }

  "stale connection" should "have attribute for extra xml element" in {
    val entry = JposEntry.fromLines(linesFrom("stale-connection.xml"))
    entry("msgType") shouldBe "receive"
    entry("stale-connection") shouldBe "Disconnecting 10.0.0.1:4242"
    entry("0") shouldBe "0800"
  }

  "header element" should "be read" in {
    val entry = JposEntry.fromLines(linesFrom("with-header.xml"))
    entry("msgType") shouldBe "receive"
    entry("header") shouldBe "00420042"
    entry("0") shouldBe "0800"
  }

  "A single entry" should "be convertible to a .csv row" in {
    val entry = JposEntry.fromLines(lines)
    entry.exprToSeq("time", "48.2.13", "11", "7").toCsv shouldEqual "16:59:03.292,subfield 48.2.13,28928,1124000003"
  }

  it should "accept expressions when converting to a .csv row" in {
    val entry = JposEntry.fromLines(lines)
    val xsv = entry.toXsv(",", { _.timestamp.toString("HH:mm:ss").substring(0, 7) + "0"}, "7")
    xsv shouldEqual "16:59:00,1124000003"
  }

  it should "accept expressions accessing subtype fields when converting to a .csv row" in {
    val entry = JposEntry.fromLines(lines)
    val xsv = entry.toXsv(",", { _.xpath("""//field[@id="7"]/@value""") }, "7")
    xsv shouldEqual "1124000003,1124000003"
  }

  "A trio of entries" should "be coalescable" in {
    val one = JposEntry("11" -> "1", "53" -> "2")
    val four = JposEntry("11" -> "4", "53" -> "1")
    val entries = Seq(one, JposEntry("11" -> "2", "53" -> "2"), JposEntry("11" -> "3", "53" -> "2"), four)
    val coalesced = JposEntry.coalesce(entries.iterator, _("53"))

    coalesced.toList shouldEqual List(one, Group(2, "2"), four)
  }

  "Attributes" should "be extracted" in {
    val attributes = JposEntry.extractAttributes("""<field id="7" value="1124000003"/>""")
    attributes should contain ("id" -> "7")
    attributes should contain ("value" -> "1124000003")
  }

  it should "extract attributes from a indented <log> line" in {
    val attributes = JposEntry.extractAttributes(s"""    <log realm="$realm" at="$timestamp" lifespan="$lifespan">""")
    attributes should contain ("realm" -> realm)
    attributes should contain ("at" -> timestamp)
    attributes should contain ("lifespan" -> lifespan)
  }

  "Id and value" should "be extracted" in {
    val attributes = JposEntry.extractIdAndValue("""<field id="7" value="1124000003"/>""", Iterator.empty)
    attributes shouldEqual ("7", "1124000003")
  }

  "An <exception> element" should "be extracted" in {
    val entry = JposEntry.fromLines(linesFrom("exception.xml"))
    entry("exception") shouldEqual "Remote host closed connection during handshake"
  }

  "An <iso-exception> element" should "be extracted" in {
    val entry = JposEntry.fromLines(linesFrom("iso-exception.xml"))
    entry("exception") shouldEqual "unconnected ISOChannel"
  }

  "Bad XML" should "be extracted" in {
    val entry = JposEntry.fromLines(linesFrom("exception-with-bad-xml.xml"))
    entry("exception") shouldBe "Sourced file: inline evaluation of: ``DATE=new Date();      MTI="
  }

  "CDATA[" should "be extracted" in {
    val entry = JposEntry.fromLines(linesFrom("with-cdata.xml"))
    entry("48.1") shouldBe """<isomsg>
                             |  <f id="0" v="0810"/>
                             |  <f id="11" v="378180"/>
                             |  <f id="39" v="00"/>
                             |</isomsg>
                             |""".stripMargin
  }

  it should "be extracted from one line" in {
    val entry = JposEntry.fromLines(linesFrom("one-line-cdata.xml"))
    entry("48.1") shouldBe "Character Data"
    entry("48.3") shouldBe "00.00"
  }

  def oneEntry(realm: String = realm, timestamp: String = timestamp, lifespan: String = lifespan, msgType: String = "receive") = {
    s"""<log realm="$realm" at="$timestamp" lifespan="$lifespan">
  <$msgType>
    <isomsg direction="incoming">
      <!-- org.jpos.iso.packager.XMLPackager -->
      <field id="0" value="0800"/>
      <field id="7" value="1124000003"/>
      <field id="11" value="28928"/>
      <isomsg id="48">
        <field id="1" value="a subfield"/>
        <isomsg id="2">
          <field id="13" value="subfield 48.2.13"/>
        </isomsg>
        <field id="3" value=""/>
      </isomsg>
    </isomsg>
  </$msgType>
</log>""".split('\n')
  }
}
