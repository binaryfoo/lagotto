package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.LogLike
import io.github.binaryfoo.isotools.shell.FieldFilter.MatchOp

trait LogFilter extends Function[LogLike, Boolean] {
  def apply(entry: LogLike): Boolean
}

case class GrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogLike): Boolean = entry.lines.contains(pattern)
}

case class NegativeGrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogLike): Boolean = !entry.lines.contains(pattern)
}

case class FieldFilter(field: String, desired: String, op: MatchOp) extends LogFilter {
  override def apply(entry: LogLike): Boolean = {
    op(entry(field), desired)
  }
}

object FieldFilter {
  type MatchOp = (String, String) => Boolean
}
