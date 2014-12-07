package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.ConvertibleToMap

trait OutputFormat {
  def header(): Option[String]
  def apply(e: ConvertibleToMap): String
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: ConvertibleToMap): String = e.lines
}

trait Delimited extends OutputFormat {
  val fields: Seq[String]

  val delimiter: String

  override def header(): Option[String] = Some(fields.mkString(delimiter))

  override def apply(e: ConvertibleToMap): String = e.toMap(fields).values.mkString(delimiter)
}

case class Tsv(fields: Seq[String]) extends Delimited {
  override val delimiter: String = "\t"
}

case class Csv(fields: Seq[String]) extends Delimited {
  override val delimiter: String = ","
}