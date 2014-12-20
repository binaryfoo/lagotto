package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.LogLike

trait OutputFormat {
  def header(): Option[String]
  def apply(e: LogLike): String
  def includesDelays: Boolean = false
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogLike): String = e.lines
}

trait Delimited extends OutputFormat {
  val fields: Seq[String]

  val delimiter: String

  override def includesDelays: Boolean = fields.contains("delay")

  override def header(): Option[String] = Some(fields.mkString(delimiter))

  override def apply(e: LogLike): String = e.toMap(fields).values.mkString(delimiter)
}

case class Tsv(fields: Seq[String]) extends Delimited {
  override val delimiter: String = "\t"
}

case class Csv(fields: Seq[String]) extends Delimited {
  override val delimiter: String = ","
}