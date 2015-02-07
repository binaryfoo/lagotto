package io.github.binaryfoo.lagotto

import org.joda.time.format.DateTimeFormat

import scala.collection.mutable

class SimpleLogEntryTest extends LagoTest {

  "An entry with a source ref" should "expose file and line number" in {
    val timeExpression = TimeExpression("timestamp", BasicJodaFormatter(DateTimeFormat.forPattern("yyyy-MM-dd")))
    val entry = SimpleLogEntry(mutable.LinkedHashMap("timestamp" -> "2015-12-30"), Some(timeExpression), "", SourceRef("file", 42))
    entry("src") shouldBe "file:42"
    entry("file") shouldBe "file"
    entry("line") shouldBe "42"
  }
}
