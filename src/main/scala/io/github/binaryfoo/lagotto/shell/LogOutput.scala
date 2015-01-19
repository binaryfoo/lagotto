package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.{FieldExpr, LogEntry}

trait OutputFormat {
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
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogEntry): Option[String] = Some(e.lines)
  override def footer(): Option[String] = None
}

case class Tabular(fields: Seq[FieldExpr], tableFormatter: TableFormatter = DelimitedTableFormat(",")) extends OutputFormat {
  override def header(): Option[String] = tableFormatter.header(fields.map(_.toString()))
  override def apply(e: LogEntry): Option[String] = tableFormatter.row(e.exprToSeq(fields))
  override def footer(): Option[String] = tableFormatter.footer()
}

trait TableFormatter {
  def header(fields: Seq[String]): Option[String]
  def row(row: Seq[String]): Option[String]
  def footer(): Option[String] = None
}

case class DelimitedTableFormat(delimiter: String) extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString(delimiter))
  override def row(row: Seq[String]): Option[String] = Some(row.mkString(delimiter))
}

object JiraTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString("||", "||", "||"))
  override def row(row: Seq[String]): Option[String] = Some(row.mkString("|", "|", "|"))
}

object HtmlTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString("<table>\n<thead><tr><th>", "</th><th>", "</th></tr></thead>\n<tbody>"))
  override def row(row: Seq[String]): Option[String] = Some(row.mkString("<tr><td>", "</td><td>", "</td></tr>"))
  override def footer(): Option[String] = Some("</tbody>\n</table>")
}

