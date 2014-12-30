package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.output.{AsciiTable, DeadSimpleJsonWriter}
import io.github.binaryfoo.lagotto.{FieldExpr, LogLike}

import scala.collection.mutable

trait OutputFormat {
  def header(): Option[String]
  def apply(e: LogLike): Option[String]
  def footer(): Option[String]
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogLike): Option[String] = Some(e.lines)
  override def footer(): Option[String] = None
}

object JSONOutput extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogLike): Option[String] = Some(DeadSimpleJsonWriter.toJson(e.toMap))
  override def footer(): Option[String] = None
}

case class Tabular(fields: Seq[FieldExpr], tableFormatter: TableFormatter = DelimitedTableFormat(",")) extends OutputFormat {
  override def header(): Option[String] = tableFormatter.header(fields.map(_.toString()))
  override def apply(e: LogLike): Option[String] = tableFormatter.row(e.exprToSeq(fields))
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

/**
 * Definitely not bounded memory use.
 */
class AsciiTableFormat extends TableFormatter {

  var fields: Seq[String] = null
  val rows = mutable.ListBuffer[Seq[String]]()

  override def header(fields: Seq[String]): Option[String] = {
    this.fields = fields
    None
  }

  override def row(row: Seq[String]): Option[String] = {
    rows += row
    None
  }

  override def footer(): Option[String] = {
    Some(new AsciiTable(maximumWidths(), rows.size)
      .addHeader(fields)
      .addRows(rows)
      .addFooter()
      .toString())
  }

  private def maximumWidths(): Seq[Int] = {
    val zeroes = Seq.fill(fields.length)(0)
    val headerAndRows = fields :: rows.toList
    headerAndRows.foldLeft(zeroes) { (maxes, row) => AsciiTable.reviseColumnWidths(row, maxes) }
  }
}

/**
 * Spits out each row as it's processed. Makes columns wider if required.
 * Not as neat but provides incremental output.
 */
class IncrementalAsciiTableFormat extends TableFormatter {

  var columnWidths: Seq[Int] = null

  override def header(fields: Seq[String]): Option[String] = {
    columnWidths = AsciiTable.reviseColumnWidths(fields, Seq.fill(fields.length)(0))
    Some(new AsciiTable(columnWidths).addHeader(fields).toIncrementalString)
  }

  override def row(row: Seq[String]): Option[String] = {
    columnWidths = AsciiTable.reviseColumnWidths(row, columnWidths)
    Some(new AsciiTable(columnWidths).addRow(row).toIncrementalString)
  }

  override def footer(): Option[String] = {
    Some(new AsciiTable(columnWidths).addFooter().toIncrementalString)
  }

}