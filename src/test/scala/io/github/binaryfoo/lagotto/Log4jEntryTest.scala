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

    entry("timestamp") shouldBe "08 Nov 2014 00:00:00,001"
    entry("level") shouldBe "INFO"
    entry("realm") shouldBe "a.ClassName"
    entry("message") shouldBe "Did something useful"
  }

  it should "export fields" in {
    val entry = Log4jEntry.fromString(oneLine)

    entry.exportAsSeq shouldBe Seq("timestamp" -> "08 Nov 2014 00:00:00,001",
      "level" -> "INFO",
      "realm" -> "a.ClassName",
      "message" -> "Did something useful")
  }

  it should "support reformatting the timestamp" in {
    val entry = Log4jEntry.fromString(oneLine)

    entry("time") shouldBe "00:00:00.001"
  }

  it should "read a two line record" in {
    val entry = Log4jEntry.fromString(twoLines)

    entry.timestamp shouldBe new DateTime(2014, 1, 11, 3, 2, 1, 999)
    entry.message shouldBe "Did something not so useful\nWith some details..."
    entry.lines shouldBe "[11 Jan 2014 03:02:01,999] ERROR [a.ClassName]: Did something not so useful\nWith some details..."
  }

  "message(/regex/replacement/)" should "apply regex to message" in {
    val entry = Log4jEntry.fromString(twoLines)
    entry.exprToSeq("message(/(^.*detail.*$)/$1/)") shouldBe Seq("With some details...")
  }
}
