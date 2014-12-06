package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.ConvertibleToMap

trait OutputFormat {
  def apply(e: ConvertibleToMap): String
}

object FullText extends OutputFormat {
  override def apply(e: ConvertibleToMap): String = e.lines.mkString("\n")
}

trait Delimited extends OutputFormat {
  val fields: Seq[String]

  val delimiter: String

  override def apply(e: ConvertibleToMap): String = e.toMap(fields).values.mkString(delimiter)
}

case class Tsv(fields: Seq[String]) extends Delimited {
  override val delimiter: String = "\t"
}

case class Csv(fields: Seq[String]) extends Delimited {
  override val delimiter: String = ","
}