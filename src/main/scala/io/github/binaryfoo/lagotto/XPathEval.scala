package io.github.binaryfoo.lagotto

import java.io.ByteArrayInputStream
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.{XPathExpression, XPathFactory}

import org.w3c.dom.Document

object XPathEval {

  def apply(xml: String, expr: String): String = compile(expr)(xml)

  def compile(path: String): HintedXPathExpression = {
    val xPathFactory = XPathFactory.newInstance()
    val xPath = xPathFactory.newXPath()
    val compiled = xPath.compile(path)
    val hint: Option[String] = path match {
      case NodeReference(node) => Some("<" + node)
      case _ => None
    }
    HintedXPathExpression(hint, compiled)
  }

  val NodeReference = "/+([^\\[/]*)[\\[/].*".r

}

case class HintedXPathExpression(hint: Option[String], compiled: XPathExpression) {

  def apply(xml: String): String = {
    if (couldApply(xml)) evaluate(XmlParser.parse(xml))
    else null
  }

  /**
   * Try to avoid parsing XML and evaluating the XPath expression.
   */
  def couldApply(xml: String): Boolean = {
    if (hint.isDefined) hint.exists(xml.contains)
    else true
  }
  
  def evaluate(doc: Document) = {
    val v = compiled.evaluate(doc)
    if (v == "") null else v
  }

}

object XmlParser {
  def parse(xml: String): Document = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(false)
    factory.setValidating(false)
    val builder = factory.newDocumentBuilder()
    builder.parse(new ByteArrayInputStream(xml.getBytes))
  }
}
