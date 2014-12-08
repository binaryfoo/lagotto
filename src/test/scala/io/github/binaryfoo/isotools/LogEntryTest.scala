package io.github.binaryfoo.isotools

import org.joda.time.DateTime
import scala.collection.immutable.ListMap
import scala.collection.mutable
import org.scalatest.{FlatSpec, Matchers}

class LogEntryTest extends FlatSpec with Matchers {

  val timestamp = "Mon Nov 24 16:59:03 EST 2014.292"
  val realm = "some.channel/10.0.0.1:4321"
  val lifespan = """10005ms"""
  val lines = oneEntry()

  "Log entry parser" should "parse extract fields" in {
    val entry = LogEntry.fromLines(lines)
    entry field "7" shouldBe "1124000003"
    entry field "48.2.13" shouldBe "subfield 48.2.13"
  }

  it should "extract realm" in {
    val entry = LogEntry.fromLines(lines)
    entry.realm shouldEqual realm
  }

  it should "extract 'at' attribute" in {
    val entry = LogEntry.fromLines(lines)
    entry.at shouldEqual timestamp
  }

  it should "parse the 'at' attribute as a timestamp" in {
    val entry = LogEntry.fromLines(lines)
    entry.timestamp shouldEqual new DateTime(2014, 11, 24, 16, 59, 3, 292)
  }

  it should "expose both fields and attributes via apply" in {
    val entry = LogEntry.fromLines(lines)

    entry("0") shouldEqual "0800"
    entry("48.2.13") shouldEqual "subfield 48.2.13"
    entry("mti") shouldEqual "0800"
    entry("at") shouldEqual timestamp
    entry("realm") shouldEqual realm
    entry("timestamp") shouldEqual "2014-11-24 16:59:03.292"
    entry("time") shouldEqual "16:59:03.292"
    entry("date") shouldEqual "2014-11-24"
    entry("42") shouldEqual null
    entry("rubbish") shouldEqual null
  }

  it should "extract logical link name from realm" in {
    val entry = LogEntry.fromLines(oneEntry(realm = "linkName.channel/10.0.0.1:4321"))
    entry("link") shouldEqual "linkName"
  }

  it should "extract type" in {
    LogEntry.fromLines(oneEntry(msgType = "receive")).msgType shouldEqual "receive"
    LogEntry.fromLines(oneEntry(msgType = "send")).msgType shouldEqual "send"
  }

  "A single entry" should "be convertible to a .csv row" in {
    val entry = LogEntry.fromLines(lines)
    entry.toCsv("time", "48.2.13", "11", "7") shouldEqual "16:59:03.292,subfield 48.2.13,28928,1124000003"
  }

  "A trio of entries" should "be coalescable" in {
    val one = LogEntry("11" -> "1", "53" -> "2")
    val four = LogEntry("11" -> "4", "53" -> "1")
    val entries = Stream(one, LogEntry("11" -> "2", "53" -> "2"), LogEntry("11" -> "3", "53" -> "2"), four)
    val coalesced = LogEntry.coalesce(entries, _("53"))

    coalesced shouldEqual List(one, Group(2, "2"), four)
  }

  "Attributes" should "be extracted" in {
    val attributes = new mutable.ListMap[String, String]
    LogEntry.extractAttributes("""<field id="7" value="1124000003"/>""", attributes)
    attributes should contain ("id" -> "7")
    attributes should contain ("value" -> "1124000003")
  }

  it should "extract attributes from a indented <log> line" in {
    val attributes = new mutable.ListMap[String, String]
    LogEntry.extractAttributes(s"""    <log realm="$realm" at="$timestamp" lifespan="$lifespan">""", attributes)
    attributes should contain ("realm" -> realm)
    attributes should contain ("at" -> timestamp)
    attributes should contain ("lifespan" -> lifespan)
  }

  "Id and value" should "be extracted" in {
    val attributes = LogEntry.extractIdAndValue("""<field id="7" value="1124000003"/>""")
    attributes shouldEqual  ("7", "1124000003")
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
