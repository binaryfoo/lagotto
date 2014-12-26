package io.github.binaryfoo.lagotto

import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

class XPathEvalTest extends FlatSpec with Matchers {

  it should "evaluate an xpath expression" in {
    val xml = Source.fromFile("src/test/resources/pool-exhaustion.xml").mkString
    XPathEval(xml, "//jobs[text()]") shouldBe "12413"
  }

  it should "return null if not found" in {
    val xml = Source.fromFile("src/test/resources/pool-exhaustion.xml").mkString
    XPathEval(xml, "//rubbish[text()]") shouldBe null
  }

  it should "match text at root" in {
    XPathEval("<root>text</root>", "//text()") shouldBe "text"
  }
}
