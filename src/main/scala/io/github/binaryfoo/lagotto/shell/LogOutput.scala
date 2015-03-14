package io.github.binaryfoo.lagotto.shell

import java.io.PrintWriter

import io.github.binaryfoo.lagotto.highlight.{AnsiMarkup, XmlHighlighter}
import io.github.binaryfoo.lagotto.{JposEntry, FieldExpr, LogEntry}

trait OutputFormat {
  def contentType: ContentType
  def header(): Option[String]
  def apply(e: LogEntry): Option[String]
  def footer(): Option[String]
}

object OutputFormat {

  def fieldsFor(f: OutputFormat): Seq[FieldExpr] = {
    f match {
      case Tabular(fields, _) => fields
      case _ => Seq()
    }
  }

  implicit class PipeToOutputFormatIterator(val it: Iterator[LogEntry]) extends AnyVal {
    def pipeTo(f: OutputFormat, out: PrintWriter) = {
      f.header().foreach(out.println)
      it.flatMap(f.apply).foreach(out.println)
      f.footer().foreach(out.println)
    }
  }
}

sealed trait ContentType {
  def mimeType: String
}
object PlainText extends ContentType {
  override val mimeType: String = "text/plain; charset=UTF-8"
}
object RichText extends ContentType {
  override val mimeType: String = "text/plain; charset=UTF-8"
}
object Html extends ContentType {
  override val mimeType: String = "text/html; charset=UTF-8"
}
object Svg extends ContentType {
  override val mimeType: String = "image/svg+xml; charset=UTF-8"
}
object Json extends ContentType {
  override val mimeType: String = "text/json; charset=UTF-8"
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogEntry): Option[String] = Some(e.lines)
  override def footer(): Option[String] = None
  override val contentType: ContentType = PlainText
}

object HighlightedText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogEntry): Option[String] = {
    e match {
      case j: JposEntry => Some(XmlHighlighter.highlight(e.lines, AnsiMarkup))
      case _ => Some(e.lines)
    }
  }
  override def footer(): Option[String] = None
  override val contentType: ContentType = PlainText
}

case class Tabular(fields: Seq[FieldExpr], tableFormatter: TableFormatter = DelimitedTableFormat(",")) extends OutputFormat {
  override def header(): Option[String] = tableFormatter.header(fields.map(_.toString()))
  override def apply(e: LogEntry): Option[String] = {
    val row = e.exprToSeq(fields)
    if (row.exists(_.nonEmpty)) {
      tableFormatter.row(row)
    } else {
      None
    }
  }
  override def footer(): Option[String] = tableFormatter.footer()
  override def contentType: ContentType = tableFormatter.contentType
}

trait TableFormatter {
  def header(fields: Seq[String]): Option[String]
  def row(row: Seq[String]): Option[String]
  def footer(): Option[String] = None
  def contentType: ContentType = PlainText
  def liveVersion: TableFormatter = this
}

case class DelimitedTableFormat(delimiter: String) extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString(delimiter))
  override def row(row: Seq[String]): Option[String] = Some(row.mkString(delimiter))
}

object DelimitedTableFormat {
  val Tsv = DelimitedTableFormat("\t")
  val Csv = DelimitedTableFormat(",")
}

object JiraTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString("||", "||", "||"))
  override def row(row: Seq[String]): Option[String] = Some(row.mkString("|", "|", "|"))
}

object HtmlTableFormat extends TableFormatter {
  private val pre =
    """<html>
      |<head>
      |<style>
      |a {
      |  text-decoration: none
      |}
      |</style>
      |</head>""".stripMargin
  private val post = "</body></html>"
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString(s"$pre\n<table>\n<thead><tr><th>", "</th><th>", "</th></tr></thead>\n<tbody>"))
  override def row(row: Seq[String]): Option[String] = Some(row.mkString("<tr><td>", "</td><td>", "</td></tr>"))
  override def footer(): Option[String] = Some(s"</tbody>\n</table>$post")
  override val contentType: ContentType = Html
}

