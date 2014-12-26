package io.github.binaryfoo.lagotto

import java.io.{ByteArrayInputStream, StringBufferInputStream}
import javax.xml.parsers.{DocumentBuilderFactory, DocumentBuilder}
import javax.xml.xpath.{XPathExpression, XPath, XPathFactory}

import org.w3c.dom.Document

object XPathEval {

  def apply(xml: String, expr: String): String = {
    compile(expr).evaluate(parse(xml))
  }

  private def compile(path: String): XPathExpression = {
    val xPathFactory = XPathFactory.newInstance()
    val xPath = xPathFactory.newXPath()
    xPath.compile(path)
  }

  private def parse(xml: String): Document = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(false)
    factory.setValidating(false)
    val builder = factory.newDocumentBuilder()
    builder.parse(new ByteArrayInputStream(xml.getBytes))
  }
}
