package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.LogLike

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

case class Tabular(fields: Seq[String], tableFormatter: TableFormatter) extends OutputFormat {
  override def header(): Option[String] = tableFormatter.header(fields)
  override def apply(e: LogLike): Option[String] = tableFormatter.row(fields, e)
  override def footer(): Option[String] = tableFormatter.footer()
}

trait TableFormatter {
  def header(fields: Seq[String]): Option[String]
  def row(fields: Seq[String], e: LogLike): Option[String]
  def footer(): Option[String] = None
}

case class DelimitedTableFormat(delimiter: String) extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString(delimiter))
  override def row(fields: Seq[String], e: LogLike): Option[String] = Some(e.toSeq(fields).mkString(delimiter))
}

object JiraTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString("||", "||", "||"))
  override def row(fields: Seq[String], e: LogLike): Option[String] = Some(e.toSeq(fields).mkString("|", "|", "|"))
}

object HtmlTableFormat extends TableFormatter {
  override def header(fields: Seq[String]): Option[String] = Some(fields.mkString("<table>\n<thead><tr><th>", "</th><th>", "</th></tr></thead>\n<tbody>"))
  override def row(fields: Seq[String], e: LogLike): Option[String] = Some(e.toSeq(fields).mkString("<tr><td>", "</td><td>", "</td></tr>"))
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

  override def row(fields: Seq[String], e: LogLike): Option[String] = {
    rows += e.toSeq(fields)
    None
  }

  override def footer(): Option[String] = {
    val b = new TableBuilder(maximumWidths())

    b.addRow(fields)
    b.addBar()
    rows.foreach(b.addRow)
    b.addBar()

    Some(b.toString())
  }

  class TableBuilder(val widths: Seq[Int]) {
    val totalWidth = widths.map(_ + 2).sum + widths.size + 1
    val bar = "=" * totalWidth + "\n"
    val b = new StringBuilder(totalWidth * (rows.size + 4), bar)

    def addBar() = b.append(bar)
    def addRow(row: Seq[String]) = {
      val padded = row.zip(widths).map { case (v, width) =>
        v + " " * (width - v.length)
      }
      padded.addString(b, "| ", " | ", " |\n")
    }

    override def toString: String = b.toString()
  }

  private def maximumWidths(): Seq[Int] = {
    val zeroes = Seq.fill(fields.length)(0)
    (fields :: rows.toList).foldLeft(zeroes) { (maxes, row) =>
      row.map(_.length).zip(maxes).map { case (w, max) => math.max(w, max)}
    }
  }
}
