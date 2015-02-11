package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.output.AsciiTable
import io.github.binaryfoo.lagotto.shell.TableFormatter

import scala.collection.mutable

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
    if (fields == null) {
      None
    } else {
      Some(new AsciiTable(maximumWidths(), rows.size)
        .addHeader(fields)
        .addRows(rows)
        .addFooter()
        .toString())
    }
  }

  private def maximumWidths(): Seq[Int] = AsciiTable.maximumWidths(fields :: rows.toList)
}
