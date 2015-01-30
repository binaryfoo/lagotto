package io.github.binaryfoo.lagotto

import org.joda.time.format.DateTimeFormat

import scala.collection.mutable

class SimpleLogEntryTest extends LagoTest {

  "An entry with a source ref" should "expose file and line number" in {
    val entry = SimpleLogEntry(mutable.LinkedHashMap("timestamp" -> "2015-12-30"), DateTimeFormat.forPattern("yyyy-MM-dd"), "", SourceRef("file", 42))
    entry("file") shouldBe "file:42"
    entry("line") shouldBe "42"
  }
}
