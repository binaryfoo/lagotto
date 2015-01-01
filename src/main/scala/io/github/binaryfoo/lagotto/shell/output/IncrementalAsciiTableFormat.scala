package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.output.AsciiTable
import io.github.binaryfoo.lagotto.output.AsciiTable.reviseColumnWidths
import io.github.binaryfoo.lagotto.shell.TableFormatter

/**
 * Spits out each row as it's processed. Makes columns wider if required.
 * Not as neat but provides incremental output.
 */
class IncrementalAsciiTableFormat extends TableFormatter {

  var columnWidths: Seq[Int] = null

  override def header(fields: Seq[String]): Option[String] = {
    columnWidths = reviseColumnWidths(fields, Seq.fill(fields.length)(0))
    Some(new AsciiTable(columnWidths).addHeader(fields).toIncrementalString)
  }

  override def row(row: Seq[String]): Option[String] = {
    columnWidths = reviseColumnWidths(row, columnWidths)
    Some(new AsciiTable(columnWidths).addRow(row).toIncrementalString)
  }

  override def footer(): Option[String] = {
    Some(new AsciiTable(columnWidths).addFooter().toIncrementalString)
  }

}
