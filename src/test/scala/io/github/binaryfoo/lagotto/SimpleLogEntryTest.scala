package io.github.binaryfoo.lagotto

import java.io.File

import org.joda.time.format.DateTimeFormat

import scala.collection.mutable

class SimpleLogEntryTest extends LagoTest {

  "An entry with a source ref" should "expose file and line number" in {
    val timeExpression = TimeExpr("timestamp", BasicJodaFormatter(DateTimeFormat.forPattern("yyyy-MM-dd")))
    val entry = SimpleLogEntry(mutable.LinkedHashMap("timestamp" -> "2015-12-30"), Some(timeExpression), "", FileRef(new File("file"), 42))
    val parser = new FieldExprParser()
    import parser.FieldExpr.expressionFor
    expressionFor("src")(entry) shouldBe "file:42"
    expressionFor("file")(entry) shouldBe "file"
    expressionFor("line")(entry) shouldBe "42"
  }
}
