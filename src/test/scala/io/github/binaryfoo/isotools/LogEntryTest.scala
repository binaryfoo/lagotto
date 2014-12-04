package io.github.binaryfoo.isotools

import org.joda.time.DateTime
import org.scalatest.{FlatSpec, Matchers}

class LogEntryTest extends FlatSpec with Matchers {

  val timestamp = "Mon Nov 24 16:59:03 EST 2014.292"
  val realm = "some.channel/10.0.0.1:4321"
  val lines = s"""<log realm="$realm" at="$timestamp" lifespan="10005ms">
  <receive>
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
  </receive>
</log>""".split('\n')

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

  "A trio of entries" should "be coalescable" in {
    val one = LogEntry("11" -> "1", "53" -> "2")
    val four = LogEntry("11" -> "4", "53" -> "1")
    val entries = List(one, LogEntry("11" -> "2", "53" -> "2"), LogEntry("11" -> "3", "53" -> "2"), four)
    val coalesced = LogEntry.coalesce(entries, _("53"))

    coalesced shouldEqual List(one, Group(2, "2"), four)
  }
}
