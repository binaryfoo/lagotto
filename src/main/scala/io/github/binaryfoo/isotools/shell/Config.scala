package io.github.binaryfoo.isotools.shell

case class Config (filters: Seq[LogFilter] = Seq(),
                   input: Seq[String] = Seq(),
                   format: OutputFormat = FullText,
                   pair: Boolean = false)
