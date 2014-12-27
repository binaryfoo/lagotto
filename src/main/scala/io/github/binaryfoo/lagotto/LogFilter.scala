package io.github.binaryfoo.lagotto

import scala.util.matching.Regex

trait LogFilter extends Function[LogLike, Boolean] {
  def apply(entry: LogLike): Boolean
}

trait FieldFilter extends LogFilter {
  def field: String
}

object FieldFilterOn {
  def unapply(f: FieldFilter): Option[String] = Some(f.field)
}

case class GrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogLike): Boolean = entry.lines.contains(pattern)
  override def toString(): String = s"grep($pattern)"
}

case class NegativeGrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogLike): Boolean = !entry.lines.contains(pattern)
  override def toString(): String = s"grep!($pattern)"
}

case class FieldOpFilter(field: String, desired: String, operatorSymbol: String, op: LogFilter.MatchOp) extends FieldFilter {
  override def apply(entry: LogLike): Boolean = {
    op(entry(field), desired)
  }
  override def toString(): String = s"$field$operatorSymbol$desired"
}

case class RegexFilter(field: String, pattern: Regex, positive: Boolean = true) extends FieldFilter {
  override def apply(entry: LogLike): Boolean = {
    val value = entry(field)
    value != null && pattern.findFirstMatchIn(value).isDefined == positive
  }
  override def toString(): String = {
    val negation = if (positive) "" else "!"
    s"$field$negation~/$pattern/"
  }
}

object LogFilter {
  type MatchOp = (String, String) => Boolean

  val LogFilterPattern = "(.+?)(!?)([=><~])([^=><~!]*)".r
  val MatchAsRegexPattern = "(.+?)(!?)~/(.+)/".r

  def unapply(s: String): Option[FieldFilter] = s match {
    case MatchAsRegexPattern(key, negation, pattern) => Some(RegexFilter(key, pattern.r, negation == ""))
    case LogFilterPattern(key, negation, operator, value) =>
      val op: MatchOp = operator match {
        case "=" => deNull(_) == _
        case ">" => greaterThanAsIntWithStringFallback
        case "<" => lessThanAsIntWithStringFallback
        case "~" => deNull(_).toLowerCase contains _.toLowerCase
      }
      if (negation == "!") {
        Some(FieldOpFilter(key, value, negation + operator, (a, b) => !op(a, b)))
      } else {
        Some(FieldOpFilter(key, value, operator, op))
      }
    case _ =>
      None
  }

  def filterFor(expr: String) = expr match {
    case LogFilter(f) => f
  }

  def deNull(s: String): String = if (s == null) "" else s

  def greaterThanAsIntWithStringFallback(left: String, right: String): Boolean = compareAsIntWithStringFallback(left, right) >= 0

  def lessThanAsIntWithStringFallback(left: String, right: String): Boolean = compareAsIntWithStringFallback(left, right) <= 0

  def compareAsIntWithStringFallback(left: String, right: String): Int = {
    val l = deNull(left)
    val r = deNull(right)
    try {
      l.toInt compare r.toInt
    }
    catch {
      case e: NumberFormatException => l compare r
    }
  }
}
