package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.LogEntry
import io.github.binaryfoo.lagotto.dictionary.DataDictionary
import io.github.binaryfoo.lagotto.highlight.{NotMarkedUp, XmlHighlighter, MarkupType}
import io.github.binaryfoo.lagotto.shell.{ContentType, OutputFormat}

case class NamedAndHighlightedFormat(dictionary: DataDictionary, markup: MarkupType) extends OutputFormat {

  private val namer = NamedAttributesFormat(dictionary)

  override def header(): Option[String] = markup.header

  override def apply(e: LogEntry): Option[String] = {
    val named = namer(e)
    markup match {
      case NotMarkedUp => named
      case _ => named.map(XmlHighlighter.highlight(_, markup))
    }
  }

  override def footer(): Option[String] = markup.footer

  override def contentType: ContentType = markup.contentType
}
