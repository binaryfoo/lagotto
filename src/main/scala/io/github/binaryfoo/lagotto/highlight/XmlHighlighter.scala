package io.github.binaryfoo.lagotto.highlight

import java.io.{PrintWriter, StringReader, StringWriter}
import javax.xml.stream.XMLStreamConstants._
import javax.xml.stream.{XMLStreamException, XMLInputFactory, XMLStreamReader}

import io.github.binaryfoo.lagotto.shell.{ContentType, Html, PlainText}

import scala.io.AnsiColor

class XmlHighlighter(out: PrintWriter, markup: MarkupType) {

  def highlight(xml: String) = {
    val factory = XMLInputFactory.newInstance()
    val reader = factory.createXMLStreamReader(new StringReader(xml))
    var started = false
    var canInline = true
    def closeIfRequired() = {
      if (started) {
        out.print(markup.startChildren)
        started = false
      }
      canInline = false
    }
    try {
      while (reader.hasNext) {
        reader.next() match {
          case START_ELEMENT =>
            closeIfRequired()
            val name = reader.getLocalName
            out.print(markup.elementStart(name))
            if (reader.getAttributeCount > 0) {
              writeAttrs(reader)
            }
            started = true
            canInline = true
          case END_ELEMENT =>
            if (canInline)
              out.print(markup.finishInlineElement)
            else
              out.print(markup.finishElementWithChildren(reader.getLocalName))
            started = false
          case CHARACTERS =>
            closeIfRequired()
            out.print(reader.getText)
          case COMMENT =>
            closeIfRequired()
            out.print(markup.comment(reader.getText))
          case _ =>
        }
      }
    }
    catch {
      case e: XMLStreamException =>
        closeIfRequired()
    }
  }

  private def writeAttrs(reader: XMLStreamReader) = {
    for (i <- 0 until reader.getAttributeCount) {
      val name = reader.getAttributeLocalName(i)
      val value = reader.getAttributeValue(i)
      out.print(" " + markup.attributeName(name) + "=" + markup.attributeValue(value))
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
  def header: Option[String] = None
  def footer: Option[String] = None
  def elementStart(name: String): String
  def attributeName(name: String): String
  def attributeValue(value: String): String
  def startChildren: String
  def finishElementWithChildren(name: String): String
  def finishInlineElement: String
  def comment(value: String): String

  def key(k: String): String
  def value(v: String): String

  def contentType: ContentType
}

object NotMarkedUp extends MarkupType {
  override def elementStart(name: String): String = "<" + name
  override def comment(value: String): String = s"<!--$value-->"
  override def finishInlineElement: String = "/>"
  override def attributeValue(value: String): String = '"' + value + '"'
  override def attributeName(name: String): String = name
  override def startChildren: String = ">"
  override def contentType: ContentType = PlainText
  override def finishElementWithChildren(name: String): String = s"</$name>"

  override def key(k: String): String = k
  override def value(v: String): String = v
}

object AnsiMarkup extends MarkupType with AnsiColor {
  override def elementStart(name: String): String = up("<" + name, YELLOW)
  override def attributeName(name: String): String = name
  override def attributeValue(value: String): String = up(s""""$value"""", GREEN)
  override def startChildren: String = up(">", YELLOW)
  override def finishElementWithChildren(name: String): String = up(s"</$name>", YELLOW)
  override def finishInlineElement: String = up("/>", YELLOW)
  override def comment(value: String): String = s"<!--$value-->"
  override val contentType: ContentType = PlainText

  override def key(k: String): String = up(k, YELLOW)
  override def value(v: String): String = up(v, GREEN)

  private def up(s: String, up: String) = s"$up$s$RESET"

}

object HtmlMarkup extends MarkupType {
  val StartTag = "&lt;"
  val EndTag = "&gt;"
  val Elem = "elem"
  val Value = "value"

  override def header: Option[String] = Some(
    """<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |<style>
       |  body {
       |    white-space: pre
       |  }
       |  .elem {
       |    color: #b58900;
       |  }
       |  .value {
       |    color: #859900;
       |  }
       |</style>
       |</head>
       |<body>
       |""".stripMargin)

  override def footer: Option[String] = Some(
    """</body>
      |</html>
      |""".stripMargin)

  override def elementStart(name: String): String = up(StartTag + name, Elem)
  override def attributeName(name: String): String = name
  override def attributeValue(value: String): String = up(s""""$value"""", Value)
  override def startChildren: String = up(EndTag, Elem)
  override def finishElementWithChildren(name: String): String = up(s"$StartTag/$name$EndTag", Elem)
  override def finishInlineElement: String = up(s"/$EndTag", Elem)
  override def comment(value: String): String = s"$StartTag!--$value--$EndTag"
  override val contentType: ContentType = Html

  override def key(k: String): String = up(k, Elem)
  override def value(v: String): String = up(v, Value)

  private def up(s: String, cssClass: String) = s"""<span class="$cssClass">$s</span>"""

}
