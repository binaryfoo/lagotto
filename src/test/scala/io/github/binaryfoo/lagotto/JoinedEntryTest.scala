package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

class JoinedEntryTest extends LagoTest {

  private val parser = new FieldExprParser()
  import parser.FieldExpr._

  "Left only entry" should "have an rtt of zero" in {
    val joined = JoinedEntry(JposEntry("at" -> JposTimestamp.format(DateTime.now())), LogEntry.empty, expressionFor("11"), ',')
    joined.rtt shouldBe 0
  }
}
