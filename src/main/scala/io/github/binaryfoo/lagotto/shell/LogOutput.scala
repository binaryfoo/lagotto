package io.github.binaryfoo.lagotto.shell

import java.io.{ByteArrayOutputStream, PrintStream}

import io.github.binaryfoo.lagotto.LogLike
import org.HdrHistogram.Histogram

trait OutputFormat {
  def header(): Option[String]
  def apply(e: LogLike): String
  def footer(): Option[String]
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogLike): String = e.lines
  override def footer(): Option[String] = None
}

case class Tabular(fields: Seq[String], tableFormatter: TableFormatter) extends OutputFormat {
  override def header(): Option[String] = Some(tableFormatter.header(fields))
  override def apply(e: LogLike): String = tableFormatter.row(fields, e)
  override def footer(): Option[String] = tableFormatter.footer()
}

trait TableFormatter {
  def header(fields: Seq[String]): String
  def row(fields: Seq[String], e: LogLike): String
  def footer(): Option[String] = None
}

case class DelimitedTableFormat(delimiter: String) extends TableFormatter {
  override def header(fields: Seq[String]): String = fields.mkString(delimiter)
  override def row(fields: Seq[String], e: LogLike): String = e.toSeq(fields).mkString(delimiter)
}

object JiraTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): String = fields.mkString("||", "||", "||")
  override def row(fields: Seq[String], e: LogLike): String = e.toSeq(fields).mkString("|", "|", "|")
}

object HtmlTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): String =  fields.mkString("<table>\n<thead><tr><th>", "</th><th>", "</th></tr></thead>\n<tbody>")
  override def row(fields: Seq[String], e: LogLike): String = e.toSeq(fields).mkString("<tr><td>", "</td><td>", "</td></tr>")
  override def footer(): Option[String] = Some("</tbody>\n</table>")
}
