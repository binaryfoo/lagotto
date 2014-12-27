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
  override def apply(e: LogLike): Option[String] = tableFormatter.row(e.toSeq(fields))
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

class AsciiTable(val columnWidths: Seq[Int], val rowCount: Int = 0) {
  
  val totalWidth = columnWidths.map(_ + 2).sum + columnWidths.size + 1
  val bar = "=" * totalWidth + "\n"
  val b = new StringBuilder(totalWidth * (rowCount + 4))

  def addHeader(fields: Seq[String]): this.type = {
    addBar()
    addRow(fields)
    addBar()
    this
  }

  def addRows(rows: mutable.Traversable[Seq[String]]): this.type = {
    rows.foreach(addRow)
    this
  }

  def addRow(row: Seq[String]): this.type  = {
    val padded = row.zip(columnWidths).map { case (v, width) =>
      v + " " * (width - v.length)
    }
    padded.addString(b, "| ", " | ", " |\n")
    this
  }

  def addFooter(): this.type = {
    addBar()
    this
  }

  def addBar() = b.append(bar)

  override def toString: String = b.toString()
  def toIncrementalString: String = {
    if (b.last == '\n') b.deleteCharAt(b.length - 1)
    b.toString()
  }
}

object AsciiTable {
  def reviseColumnWidths(row: Seq[String], currentWidths: Seq[Int]): Seq[Int] = {
    row.map(_.length).zip(currentWidths).map { case (w, max) => math.max(w, max)}
  }
}
