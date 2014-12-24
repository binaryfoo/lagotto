package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.LogLike

trait OutputFormat {
  def header(): Option[String]
  def apply(e: LogLike): String
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogLike): String = e.lines
}

case class Tabular(fields: Seq[String], tableFormatter: TableFormatter) extends OutputFormat {
  override def header(): Option[String] = Some(tableFormatter.header(fields))
  override def apply(e: LogLike): String = tableFormatter.row(fields, e)
}

trait TableFormatter {
  def header(fields: Seq[String]): String
  def row(fields: Seq[String], e: LogLike): String
}

case class DelimitedTableFormat(delimiter: String) extends TableFormatter {
  override def header(fields: Seq[String]): String = fields.mkString(delimiter)
  override def row(fields: Seq[String], e: LogLike): String = e.toSeq(fields).mkString(delimiter)
}

object JiraTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): String = fields.mkString("||", "||", "||")
  override def row(fields: Seq[String], e: LogLike): String = e.toSeq(fields).mkString("|", "|", "|")
}
