package io.github.binaryfoo.lagotto.highlight

import java.io.{FileWriter, PrintWriter, StringWriter}

import io.github.binaryfoo.lagotto.LagoTest

class XmlHighlighterTest extends LagoTest {

  "ANSI output format" should "highlight" in {
    val xml = contentsOf("one.xml")
    val highlighted = XmlHighlighter.highlight(xml, AnsiMarkup)
    highlighted shouldBe contentsOf("expected-ansi-one.txt").dropRight(1) // drop trailing newline
  }

  "HTML output format" should "highlight" in {
    val xml = contentsOf("one.xml")
    val highlighted = XmlHighlighter.highlight(xml, HtmlMarkup)
    highlighted shouldBe contentsOf("expected-html-one.html").dropRight(1) // drop trailing newline
  }

  "Null output format" should "do nothing" in {
    val xml = contentsOf("one.xml")
    val highlighted = XmlHighlighter.highlight(xml, NotMarkedUp)
    highlighted shouldBe contentsOf("one.xml").dropRight(1) // drop trailing newline
  }

  it should "handle just a start tag" in {
    val xml = """<log realm="terminal-server-1-newCA.server.channel/203.39.124.193:22740" at="Wed Dec 10 13:56:24 EST 2014.106">"""
    val highlighted = XmlHighlighter.highlight(xml, NotMarkedUp)
    highlighted shouldBe xml
  }
}
