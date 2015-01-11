package io.github.binaryfoo.lagotto

import scala.util.matching.Regex

trait LogFilter extends Function[LogLike, Boolean] {
  def apply(entry: LogLike): Boolean
}

trait FieldFilter extends LogFilter {
  def field: String = expr.toString()
  def expr: FieldExpr
}

object FieldFilterOn {
  def unapply(f: FieldFilter): Option[FieldExpr] = Some(f.expr)
}

case class GrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogLike): Boolean = entry.lines.contains(pattern)
  override def toString(): String = s"grep($pattern)"
}

case class NegativeGrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogLike): Boolean = !entry.lines.contains(pattern)
  override def toString(): String = s"grep!($pattern)"
}

case class InsensitiveGrepFilter(pattern: String) extends LogFilter {
  val lowerPattern = pattern.toLowerCase
  override def apply(entry: LogLike): Boolean = entry.lines.toLowerCase.contains(lowerPattern)
  override def toString(): String = s"igrep($pattern)"
}

case class NegativeInsensitiveGrepFilter(pattern: String) extends LogFilter {
  val lowerPattern = pattern.toLowerCase
  override def apply(entry: LogLike): Boolean = !entry.lines.toLowerCase.contains(lowerPattern)
  override def toString(): String = s"igrep!($pattern)"
}

case class FieldOpFilter(expr: FieldExpr, desired: String, operatorSymbol: String, op: LogFilter.MatchOp) extends FieldFilter {
  override def apply(entry: LogLike): Boolean = {
    op(expr(entry), desired)
  }
  override def toString(): String = s"$expr$operatorSymbol$desired"
}

case class AndFilter(filters: Seq[LogFilter]) extends LogFilter {
  override def apply(entry: LogLike): Boolean = filters.forall(_.apply(entry))
  override def toString(): String = filters.mkString(",")
}

object AllFilter extends LogFilter {
  override def apply(entry: LogLike): Boolean = true
  override def toString(): String = "all"
}

case class RegexFilter(expr: FieldExpr, pattern: Regex, positive: Boolean = true) extends FieldFilter {
  override def apply(entry: LogLike): Boolean = {
    val value = expr(entry)
    value != null && pattern.findFirstMatchIn(value).isDefined == positive
  }
  override def toString(): String = {
    val negation = if (positive) "" else "!"
    s"$expr$negation~/$pattern/"
  }
}

object LogFilter {
  type MatchOp = (String, String) => Boolean

  val LogFilterPattern = "(.+?)(!?)([=><~])([^=><~!]*)".r
  val MatchAsRegexPattern = "(.+?)(!?)~/(.+)/".r

  def unapply(s: String): Option[FieldFilter] = s match {
    case MatchAsRegexPattern(FieldExpr(expr), negation, pattern) => Some(RegexFilter(expr, pattern.r, negation == ""))
    case LogFilterPattern(FieldExpr(expr), negation, operator, value) =>
      val op: MatchOp = operator match {
        case "=" => equalsOp
        case ">" => greaterThanAsIntWithStringFallback
        case "<" => lessThanAsIntWithStringFallback
        case "~" => containsOp
      }
      if (negation == "!") {
        Some(FieldOpFilter(expr, value, negation + operator, (a, b) => !op(a, b)))
      } else {
        Some(FieldOpFilter(expr, value, operator, op))
      }
    case _ =>
      None
  }

  def filterFor(expr: String) = expr match {
    case LogFilter(f) => f
  }

  def deNull(s: String): String = if (s == null) "" else s

  // Need vals for LogFilter equality to work
  val equalsOp = (a: String, b: String) => deNull(a) == b
  val containsOp = (a: String, b: String) => deNull(a).toLowerCase contains b.toLowerCase
  val greaterThanAsIntWithStringFallback = (left: String, right: String) => compareAsIntWithStringFallback(left, right) >= 0
  val lessThanAsIntWithStringFallback = (left: String, right: String) => compareAsIntWithStringFallback(left, right) <= 0

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

object AndFilter {

  def unapply(commaSeparated: String): Option[AndFilter] = {
    val children = commaSeparated.split(',').flatMap(LogFilter.unapply)
    if (children.isEmpty) None
    else Some(AndFilter(children))
  }

  def from(commaSeparated: String): AndFilter = unapply(commaSeparated).get
}
