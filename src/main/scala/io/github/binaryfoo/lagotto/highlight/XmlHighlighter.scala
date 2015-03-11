package io.github.binaryfoo.lagotto.highlight

import java.io.{StringWriter, PrintWriter}

import io.github.binaryfoo.lagotto.XmlParser
import org.w3c.dom.{NamedNodeMap, Node, NodeList}

import scala.io.AnsiColor

class XmlHighlighter(out: PrintWriter, markup: MarkupType) {

  def highlight(xml: String) = {
    val doc = XmlParser.parse(xml)
    val children = doc.getChildNodes
    write(children, 0)
  }

  def write(children: NodeList, depth: Int): Unit = {
    for (i <- 0 until children.getLength) {
      val node = children.item(i)
      node.getNodeType match {
        case Node.ELEMENT_NODE =>
          val name = node.getNodeName
          out.print(markup.elementStart(name))
          if (node.hasAttributes) {
            writeAttrs(node.getAttributes)
          }
          if (node.hasChildNodes) {
            out.print(markup.startChildren)
            write(node.getChildNodes, depth + 1)
            out.print(markup.finishElementWithChildren(name))
          } else {
            out.print(markup.finishInlineElement)
          }
        case Node.TEXT_NODE =>
          out.print(node.getTextContent)
        case Node.COMMENT_NODE =>
          out.print(markup.comment(node.getTextContent))
        case _ =>
      }
    }
  }
  
  def writeAttrs(attrs: NamedNodeMap) = {
    for (i <- 0 until attrs.getLength) {
      val attr = attrs.item(i)
      val name = attr.getNodeName
      val value = attr.getNodeValue
      out.print(" " + markup.attributeName(name) + markup.attributeValue(value))
    }
  }
}

object XmlHighlighter {
  def highlight(xml: String, markup: MarkupType): String = {
    val result = new StringWriter()
    val out = new PrintWriter(result)
    new XmlHighlighter(out, markup).highlight(xml)
    out.close()
    result.toString
  }
}

trait MarkupType {
  def elementStart(name: String): String
  def attributeName(name: String): String
  def attributeValue(value: String): String
  def startChildren: String
  def finishElementWithChildren(name: String): String
  def finishInlineElement: String
  def comment(value: String): String
}

object AnsiMarkup extends MarkupType with AnsiColor {
  override def elementStart(name: String): String = up("<" + name, YELLOW)
  override def attributeName(name: String): String = name + "="
  override def attributeValue(value: String): String = up(s""""$value"""", GREEN)
  override def startChildren: String = up(">", YELLOW)
  override def finishElementWithChildren(name: String): String = up(s"</$name>", YELLOW)
  override def finishInlineElement: String = up("/>", YELLOW)
  override def comment(value: String): String = s"<!-- $value -->"

  private def up(s: String, up: String) = s"$up$s$RESET"

}
