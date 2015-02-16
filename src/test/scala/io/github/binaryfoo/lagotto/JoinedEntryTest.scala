package io.github.binaryfoo.lagotto

import org.joda.time.DateTime
import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension

class JoinedEntryTest extends LagoTest {

  private val parser = new FieldExprParser()
  import parser.FieldExpr._

  "Left only entry" should "have an rtt of zero" in {
    val joined = JoinedEntry(JposEntry("at" -> JposTimestamp.format(DateTime.now())), LogEntry.empty, expressionFor("11"), ',')
    joined.rtt shouldBe 0
  }

  "Lines of joined jpos entries" should "show full xml" in {
    val now = DateTime.now()
    val left = JposEntry(lines = "<xml>left</xml>", "at" -> now.asJposAt)
    val right = JposEntry(lines = "<xml>right</xml>", "at" -> now.plusMinutes(5).asJposAt)
    val joined = JoinedEntry(left, right, expressionFor("11"), '\n')
    joined.lines shouldBe
      """<xml>left</xml>
        |<xml>right</xml>""".stripMargin
  }
}
