package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.LogFilters.MatchOp
import io.github.binaryfoo.lagotto.reader.FileIO

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex

trait LogFilter extends Function[LogEntry, Boolean] {
  def apply(entry: LogEntry): Boolean
}

trait FieldFilter extends LogFilter {
  def field: String = expr.toString()

  def expr: FieldExpr
}

object FieldFilterOn {
  def unapply(f: FieldFilter): Option[FieldExpr] = Some(f.expr)
}

case class GrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogEntry): Boolean = entry.lines.contains(pattern)

  override def toString(): String = s"grep($pattern)"
}

case class NegativeGrepFilter(pattern: String) extends LogFilter {
  override def apply(entry: LogEntry): Boolean = !entry.lines.contains(pattern)

  override def toString(): String = s"grep!($pattern)"
}

case class InsensitiveGrepFilter(pattern: String) extends LogFilter {
  val lowerPattern = pattern.toLowerCase

  override def apply(entry: LogEntry): Boolean = entry.lines.toLowerCase.contains(lowerPattern)

  override def toString(): String = s"igrep($pattern)"
}

case class NegativeInsensitiveGrepFilter(pattern: String) extends LogFilter {
  val lowerPattern = pattern.toLowerCase

  override def apply(entry: LogEntry): Boolean = !entry.lines.toLowerCase.contains(lowerPattern)

  override def toString(): String = s"igrep!($pattern)"
}

case class FieldOpFilter(expr: FieldExpr, desired: String, operatorSymbol: String, op: MatchOp) extends FieldFilter {
  override def apply(entry: LogEntry): Boolean = {
    op(expr(entry), desired)
  }

  override def toString(): String = s"$expr$operatorSymbol$desired"
}

case class AndFilter(filters: Seq[LogFilter]) extends LogFilter {
  override def apply(entry: LogEntry): Boolean = filters.forall(_.apply(entry))

  override def toString(): String = filters.mkString(",")
}

object AllFilter extends LogFilter {
  override def apply(entry: LogEntry): Boolean = true

  override def toString(): String = "all"
}

case class RegexFilter(expr: FieldExpr, pattern: Regex, positive: Boolean = true) extends FieldFilter {
  override def apply(entry: LogEntry): Boolean = {
    val value = expr(entry)
    value != null && pattern.findFirstMatchIn(value).isDefined == positive
  }

  override def toString(): String = {
    val negation = if (positive) "" else "!"
    s"$expr$negation~/$pattern/"
  }
}

case class InSetFilter(expr: FieldExpr, values: Set[String]) extends FieldFilter {
  override def apply(entry: LogEntry): Boolean = values.contains(expr(entry))

  override def toString(): String = s"$expr in (${values.mkString(",")})"
}

case class NotInSetFilter(expr: FieldExpr, values: Set[String]) extends FieldFilter {
  override def apply(entry: LogEntry): Boolean = !values.contains(expr(entry))

  override def toString(): String = s"$expr not in (${values.mkString(",")})"
}

/**
 * Match messages matching ``filter`` and any messages other messages from the same channel (realm).
 * Until the channel is closed.
 */
class ChannelWith(filter: LogFilter) extends LogFilter {
  private val channels = mutable.HashSet[String]()

  override def apply(entry: LogEntry): Boolean = {
    val realm = entry("realm")
    if (filter(entry)) {
      if (realm != null)
        channels += realm
      true
    } else {
      val hit = channels.contains(realm)
      if (hit) {
        if (entry("msgType") == "session-end")
          channels -= realm
      }
      hit
    }
  }
}

object LogFilters {
  type MatchOp = (String, String) => Boolean

  /**
   * Applies no translations.
   */
  val NaiveParser = new LogFilterParser(new FieldExprParser())

  // Need vals for LogFilter equality to work
  val equalsOp = (a: String, b: String) => deNull(a) == b
  val containsOp = (a: String, b: String) => deNull(a).toLowerCase contains b.toLowerCase
  val greaterThanAsIntWithStringFallback = (left: String, right: String) => compareAsIntWithStringFallback(left, right) >= 0
  val lessThanAsIntWithStringFallback = (left: String, right: String) => compareAsIntWithStringFallback(left, right) <= 0

  private def compareAsIntWithStringFallback(left: String, right: String): Int = {
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

/**
 * Parse filters where the field expressions can depend on dictionary translations.
 */
class LogFilterParser(val fieldParser: FieldExprParser) {

  import fieldParser.FieldExpr
  import LogFilters._

  object LogFilter {

    private val LogFilterPattern = "(.+?)(!?)([=><~])([^=><~!]*)".r
    private val MatchAsRegexPattern = "(.+?)(!?)~/(.+)/".r
    private val GrepPattern = """grep\(([^)]+?)\)""".r
    private val GrepNotPattern = """grep!\(([^)]+?)\)""".r
    private val IGrepPattern = """igrep\(([^)]+?)\)""".r
    private val IGrepNotPattern = """igrep!\(([^)]+?)\)""".r
    private val NotInPattern = """(.+) not in \(([^)]+)\)""".r
    private val NotInFilePattern = """(.+) not in file "([^)]+)"""".r
    private val InPattern = """(.+) in \(([^)]+)\)""".r
    private val InFilePattern = """(.+) in file "([^"]+)"""".r
    private val ChannelWith = """channelWith\((.+)\)""".r

    def unapply(s: String): Option[LogFilter] = s match {
      case MatchAsRegexPattern(FieldExpr(expr), negation, pattern) => Some(RegexFilter(expr, pattern.r, negation == ""))
      case ChannelWith(LogFilter(filter)) => Some(new ChannelWith(filter))
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
      case GrepPattern(text) => Some(GrepFilter(text))
      case GrepNotPattern(text) => Some(NegativeGrepFilter(text))
      case IGrepPattern(text) => Some(InsensitiveGrepFilter(text))
      case IGrepNotPattern(text) => Some(NegativeInsensitiveGrepFilter(text))
      case NotInPattern(FieldExpr(expr), list) => Some(NotInSetFilter(expr, list.split(',').toSet))
      case NotInFilePattern(FieldExpr(expr), file) => Some(NotInSetFilter(expr, readToSet(file)))
      case InPattern(FieldExpr(expr), list) => Some(InSetFilter(expr, list.split(',').toSet))
      case InFilePattern(FieldExpr(expr), file) => Some(InSetFilter(expr, readToSet(file)))
      case _ =>
        None
    }

    def filterFor(expr: String): FieldFilter = expr match {
      case LogFilter(f) if f.isInstanceOf[FieldFilter] => f.asInstanceOf[FieldFilter]
    }

    private def readToSet(f: String): Set[String] = {
      val source = Source.fromFile(f)
      try {
        source.getLines().toSet
      }
      finally {
        source.close()
      }
    }

  }

  def parseAndExpr(commaSeparated: String): Option[AndFilter] = {
    val children = commaSeparated.split(',').flatMap(LogFilter.unapply)
    if (children.isEmpty) None
    else Some(AndFilter(children))
  }

}

