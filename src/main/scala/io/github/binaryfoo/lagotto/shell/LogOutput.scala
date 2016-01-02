package io.github.binaryfoo.lagotto.shell

import java.io.PrintWriter

import io.github.binaryfoo.lagotto.highlight.{AnsiMarkup, XmlHighlighter}
import io.github.binaryfoo.lagotto._

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

trait FieldList {
  def fields: Seq[FieldExpr]
  def fieldNames: Seq[String] = fields.map(_.toString())
}

case class Tabular(fields: Seq[FieldExpr], tableFormatter: TableFormatter = DelimitedTableFormat(",")) extends OutputFormat with FieldList {
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

case class WildcardTable(tableFormatter: TableFormatter = DelimitedTableFormat(",")) extends OutputFormat with FieldList {
  var fields: Seq[FieldExpr] = null
  override def header(): Option[String] = {
    if (fields == null) {
      throw new IAmSorryDave("Need at least one output row before printing header")
    }
    tableFormatter.header(fieldNames)
  }
  override def apply(e: LogEntry): Option[String] = {
    val row = if (fields == null) {
      val (names: Seq[String], values: Seq[String]) = e.exportAsSeq.unzip(p => (p._1, p._2))
      fields = names.map(PrimitiveExpr)
      values
    } else {
      e.exprToSeq(fields)
    }
    tableFormatter.row(row)
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
  override def header(fields: Seq[String]): Option[String] = Some(fields.map(_.replace(delimiter, " ")).mkString(delimiter))
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

case class InfluxDBFormat(measurement: String = "", tags: Seq[FieldExpr] = Seq.empty, fields: Seq[FieldExpr] = Seq.empty) extends OutputFormat {
  override def contentType: ContentType = PlainText
  override def footer(): Option[String] = None
  override def header(): Option[String] = None

  def escape(s: String): String = {
    if (s == null) {
      s
    } else {
      s.replaceAll(" ", "\\\\ ")
    }
  }

  override def apply(e: LogEntry): Option[String] = {
    val tagsAndValues = tags.map(f => f.field + "=" + escape(f(e))).mkString(",")
    val fieldsAndValues = fields.map(f => f.field + "=" + escape(f(e))).mkString(",")
    val nanoTimestamp = e.timestamp.getMillis * 1000 * 1000
    Some(s"$measurement,$tagsAndValues $fieldsAndValues $nanoTimestamp")
  }
}
