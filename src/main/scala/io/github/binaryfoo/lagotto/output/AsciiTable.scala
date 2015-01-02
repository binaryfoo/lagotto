package io.github.binaryfoo.lagotto.output

/**
 * Make a table out of = and | symbols.
 */
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

  def addRows(rows: Seq[Seq[String]]): this.type = {
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

  /**
   * No Trailing newline character.
   */
  def toIncrementalString: String = {
    if (b.last == '\n') b.deleteCharAt(b.length - 1)
    b.toString()
  }
}

object AsciiTable {

  def from(header: Seq[String] = Seq(), rows: Seq[Seq[String]]): AsciiTable = {
    val allRows = if (header.nonEmpty) Seq(header) ++ rows else rows
    val table = new AsciiTable(maximumWidths(allRows))
    if (header.nonEmpty) {
      table.addHeader(header)
    }
    rows.foreach(table.addRow)
    table.addFooter()
    table
  }

  def from(rows: Seq[Seq[String]]): AsciiTable = {
    val table = new AsciiTable(maximumWidths(rows))
    table.addBar()
    rows.foreach(table.addRow)
    table.addFooter()
    table
  }

  def maximumWidths(rows: Seq[Seq[String]]): Seq[Int] = {
    val zeroes = Seq.fill(rows.head.length)(0)
    rows.foldLeft(zeroes) { (maxes, row) => reviseColumnWidths(row, maxes) }
  }

  def reviseColumnWidths(row: Seq[String], currentWidths: Seq[Int]): Seq[Int] = {
    row.map(_.length).zip(currentWidths).map { case (w, max) => math.max(w, max)}
  }
}