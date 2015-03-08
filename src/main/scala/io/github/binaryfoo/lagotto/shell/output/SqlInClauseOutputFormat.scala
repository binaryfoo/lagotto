package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.{FieldExpr, LogEntry}
import io.github.binaryfoo.lagotto.shell.{TableFormatter, OutputFormat}

class SqlInClauseOutputFormat() extends TableFormatter {
  
  private var first = true
  
  override def footer(): Option[String] = Some(")")

  override def header(fields: Seq[String]): Option[String] = Some("(")

  override def row(row: Seq[String]): Option[String] = {
    val quoted = row.map(v => s"'$v'")
    val value = if (quoted.size == 1) {
      quoted.head
    } else {
      quoted.mkString("(", ",", ")")
    }
    Some(if (first) {
      first = false
      value
    } else {
      "," + value
    })
  }
}
