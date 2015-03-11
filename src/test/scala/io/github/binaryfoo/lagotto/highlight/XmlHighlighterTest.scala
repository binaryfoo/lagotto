package io.github.binaryfoo.lagotto.highlight

import java.io.{FileWriter, PrintWriter, StringWriter}

import io.github.binaryfoo.lagotto.LagoTest

class XmlHighlighterTest extends LagoTest {

  "ANSI output format" should "highlight" in {
    val xml = contentsOf("one.xml")
    val highlighted = XmlHighlighter.highlight(xml, AnsiMarkup)
    highlighted shouldBe contentsOf("expected-ansi-one.txt").dropRight(1) // drop trailing newline
  }
}
