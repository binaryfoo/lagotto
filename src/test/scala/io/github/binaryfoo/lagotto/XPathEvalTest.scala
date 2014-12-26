package io.github.binaryfoo.lagotto

import org.scalatest.{Matchers, FlatSpec}

import scala.io.Source

class XPathEvalTest extends FlatSpec with Matchers {

  it should "evaluate an xpath expression" in {
    val xml = Source.fromFile("src/test/resources/pool-exhaustion.xml").mkString
    XPathEval(xml, "//jobs[text()]") shouldBe "12413"
  }
}
