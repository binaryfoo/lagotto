package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

class Log4jEntryTest extends LagoTest {

  private val oneLine = "[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful"
  private val twoLines = "[11 Jan 2014 03:02:01,999] ERROR [a.ClassName]: Did something not so useful\nWith some details..."

  "fromString()" should "read a single line record" in {
    val entry = Log4jEntry.fromString(oneLine)

    entry.timestamp shouldBe new DateTime(2014, 11, 8, 0, 0, 0, 1)
    entry.level shouldBe "INFO"
    entry.realm shouldBe "a.ClassName"
    entry.message shouldBe "Did something useful"
  }

  it should "expose fields through apply()" in {
    val entry = Log4jEntry.fromString(oneLine)

    entry("timestamp") shouldBe "2014-11-08 00:00:00.001"
    entry("level") shouldBe "INFO"
    entry("category") shouldBe "a.ClassName"
    entry("message") shouldBe "Did something useful"
  }

  it should "export fields" in {
    val entry = Log4jEntry.fromString(oneLine)

    entry.exportAsSeq shouldBe Seq("timestamp" -> "2014-11-08 00:00:00.001",
      "level" -> "INFO",
      "category" -> "a.ClassName",
      "payload" -> "Did something useful")
  }

  it should "normalize the timestamp format" in {
    val entry = Log4jEntry.fromString(oneLine)

    entry("timestamp") shouldBe "2014-11-08 00:00:00.001"
  }

  it should "read a two line record" in {
    val entry = Log4jEntry.fromString(twoLines)

    entry.timestamp shouldBe new DateTime(2014, 1, 11, 3, 2, 1, 999)
    entry.payload shouldBe "Did something not so useful\nWith some details..."
    entry.message shouldBe "Did something not so useful"
    entry.lines shouldBe "[11 Jan 2014 03:02:01,999] ERROR [a.ClassName]: Did something not so useful\nWith some details..."
  }

  "payload(/regex/replacement/)" should "apply regex to payload" in {
    import fieldParser.stringAsFieldAccessor
    val entry = Log4jEntry.fromString(twoLines)
    entry.exprToSeq("payload(/(^.*detail.*$)/$1/)") shouldBe Seq("With some details...")
  }

  "An entry containing a jpos entry" should "expose the jpos entry" in {
    import fieldParser.FieldExpr.expressionFor
    val text = """[08 Nov 2014 00:00:20,529] ERROR [some.package]: <log realm="some.channel/172.0.1.7:4779" at="Sat Nov 08 00:00:21 EST 2014.529" lifespan="290ms">
                 |  <receive>
                 |    <exception name="Oops">
                 |    Oops
                 |    </exception>
                 |  </receive>
                 |</log>""".stripMargin
    val entry = Log4jEntry.fromString(text)

    expressionFor("jpos.exception")(entry) shouldBe "Oops"
    entry("exception") shouldBe "Oops"
    entry("realm") shouldBe "some.channel/172.0.1.7:4779"
    entry("ipAddress") shouldBe "172.0.1.7"
    expressionFor("jpos.timestamp")(entry) shouldBe "2014-11-08 00:00:21.529"
    expressionFor("timestamp")(entry) shouldBe "2014-11-08 00:00:20.529"
    entry("rubbish") shouldBe null
  }

  "custom parseDateTime" should "work" in {
    Log4jEntry.parseDateTime("28 Aug 2014 00:00:00,002") shouldBe new DateTime(2014, 8, 28, 0, 0, 0, 2)
    Log4jEntry.parseDateTime("28 Aug 2014 01:02:03,004") shouldBe new DateTime(2014, 8, 28, 1, 2, 3, 4)
    Log4jEntry.parseDateTime("1 Jan 2015 13:14:15,999") shouldBe new DateTime(2015, 1, 1, 13, 14, 15, 999)
  }
}
