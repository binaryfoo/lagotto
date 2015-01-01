package io.github.binaryfoo.lagotto

class XPathEvalTest extends LagoTest {

  it should "evaluate an xpath expression" in {
    val xml = contentsOf("pool-exhaustion.xml")
    XPathEval(xml, "//jobs[text()]") shouldBe "12413"
  }

  it should "return null if not found" in {
    val xml = contentsOf("pool-exhaustion.xml")
    XPathEval(xml, "//rubbish[text()]") shouldBe null
  }

  it should "match text at root" in {
    XPathEval("<root>text</root>", "//text()") shouldBe "text"
  }

  it should "match an attribute value" in {
    val xml = contentsOf("basic.xml")
    XPathEval(xml, """//field[@id="7"]/@value""") shouldBe "1124000003"
  }
}
