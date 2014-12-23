package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.LogLike

trait OutputFormat {
  def header(): Option[String]
  def apply(e: LogLike): String
}

object FullText extends OutputFormat {
  override def header(): Option[String] = None
  override def apply(e: LogLike): String = e.lines
}

case class Delimited(fields: Seq[String], delimiter: String) extends OutputFormat {
  override def header(): Option[String] = Some(fields.mkString(delimiter))
  override def apply(e: LogLike): String = e.toSeq(fields).mkString(delimiter)
}