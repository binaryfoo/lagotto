package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.ConvertibleToMap

trait LogFilter {
  def apply(entry: ConvertibleToMap): Boolean
}

case class GrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: ConvertibleToMap): Boolean = entry.lines.exists(_.contains(pattern))
}

case class FieldFilter(field: String, value: String) extends LogFilter {
  override def apply(entry: ConvertibleToMap): Boolean = entry(field) == value
}
