package io.github.binaryfoo.lagotto

import java.io.ByteArrayInputStream
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.xpath.{XPathExpression, XPathFactory}

import org.w3c.dom.Document

import scala.collection.mutable

object XPathEval {

//  System.setProperty("org.apache.xml.dtm.DTMManager", "org.apache.xml.dtm.ref.DTMManagerDefault")

  // should bound in size or be weak
  private val cache = mutable.Map[String, HintedXPathExpression]()

  def apply(xml: String, expr: String): String = {
    val expression = cache.getOrElseUpdate(expr, compile(expr))
    if (expression.couldApply(xml)) expression.evaluate(parse(xml))
    else null
  }

  private def compile(path: String): HintedXPathExpression = {
    val xPathFactory = XPathFactory.newInstance()
    val xPath = xPathFactory.newXPath()
    val compiled = xPath.compile(path)
    val hint: Option[String] = path match {
      case NodeReference(node) => Some("<" + node)
      case _ => None
    }
    HintedXPathExpression(hint, compiled)
  }

  private def parse(xml: String): Document = {
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(false)
    factory.setValidating(false)
    val builder = factory.newDocumentBuilder()
    builder.parse(new ByteArrayInputStream(xml.getBytes))
  }

  val NodeReference = ".*/(.*)[\\[/].*".r

}

case class HintedXPathExpression(hint: Option[String], compiled: XPathExpression) {

  def couldApply(xml: String): Boolean = {
    if (hint.isDefined) hint.exists(xml.contains)
    else true
  }
  
  def evaluate(doc: Document) = {
    val v = compiled.evaluate(doc)
    if (v == "") null else v
  }
}
