package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.ConvertibleToMap
import io.github.binaryfoo.isotools.shell.FieldFilter.MatchOp

trait LogFilter {
  def apply(entry: ConvertibleToMap): Boolean
}

case class GrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: ConvertibleToMap): Boolean = entry.lines.exists(_.contains(pattern) )
}

case class NegativeGrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: ConvertibleToMap): Boolean = !entry.lines.exists(_.contains(pattern))
}

case class FieldFilter(field: String, desired: String, op: MatchOp) extends LogFilter {
  override def apply(entry: ConvertibleToMap): Boolean = {
    op(entry(field), desired)
  }
}

object FieldFilter {
  type MatchOp = (String, String) => Boolean
}
