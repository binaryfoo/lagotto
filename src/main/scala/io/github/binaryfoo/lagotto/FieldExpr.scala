package io.github.binaryfoo.lagotto

import java.io.File
import java.text.DecimalFormat
import java.util.regex.Pattern

import io.github.binaryfoo.lagotto.ConvertExpr.TimeConversionOp
import io.github.binaryfoo.lagotto.RenderHint.RenderHint
import io.github.binaryfoo.lagotto.dictionary.DataDictionary
import org.joda.time.Period

import scala.collection.mutable
import scala.language.implicitConversions

/**
 * Needed to be able to parse expressions using the dictionary for translations.
 */
case class FieldExprParser(dictionary: Option[DataDictionary] = None, renderHints: Set[RenderHint] = Set.empty) {

  private val logFilterParser = new LogFilterParser(this)

  object FieldExpr {

    import logFilterParser.LogFilter

    val SubtractOp = """calc\((.+)-(.+)\)""".r
    val DivideOp = """calc\((.+)/(.+)\)""".r
    val ConvertOp = """\(([^ ]+) (?:(.+) )?as (.+)\)""".r
    val TranslateOp = """translate\((.+)\)""".r
    val RegexReplacementOp = """([^(]+)\(/(.+)/(.*)/\)""".r
    val PivotOp = """pivot\((.+)\)""".r
    val ResultOfPivotOp = """pivoted\((.+)\)""".r
    val XPathAccess = """xpath\((.+)\)""".r
    val Alias = """(.*) as "([^"]*)"""".r
    val LengthOf = """length\((.+)\)""".r
    val ElapsedSince = """elapsedSince\((.+)\)""".r

    def unapply(expr: String): Option[FieldExpr] = {
      Some(expr match {
        case Alias(FieldExpr(target), alias) => AliasExpr(target, alias)
        case SubtractOp(FieldExpr(left), FieldExpr(right)) => SubtractExpr(expr, left, right)
        case DivideOp(FieldExpr(left), FieldExpr(right)) => DivideExpr(expr, left, right)
        case ConvertOp(FieldExpr(child), from, to) => ConvertExpr(expr, child, from, to)
        case "delay" => DelayExpr
        case "elapsed" => ElapsedExpr()
        case ElapsedSince(LogFilter(condition)) => ElapsedSinceExpr(expr, condition)
        case AggregateOp(op) => AggregateExpr(expr, op)
        case TranslateOp(field) => TranslateExpr(expr, field, dictionary.getOrElse(throw new IAmSorryDave(s"No dictionary configured. Can't translate '$expr'")))
        case RegexReplacementOp(p@DirectExpr(path), regex, replacement) => RegexReplaceExpr(expr, path, regex, replacement)
        case PivotOp(p@DirectExpr(pivot)) => PivotExpr(p, pivot)
        case ResultOfPivotOp(p@FieldExpr(field)) => PivotResultExpr(expr, p)
        case XPathAccess(xpath) => XPathExpr(expr, xpath)
        case TimeFormatter(formatter) => TimeExpr(expr, formatter)
        case "src" if renderHints.contains(RenderHint.Html) => SourceHrefExpr
        case "src" => SourceExpr
        case "file" => FileExpr
        case "line" => LineExpr
        case "icon" if renderHints.contains(RenderHint.Html) => HtmlIconExpr
        case "icon" if renderHints.contains(RenderHint.RichText) => UnicodeIconExpr
        case "icon" => AsciiIconExpr
        case "lines" => LinesExpr
        case "summary" => SummaryExpr(expressionFor("icon"), dictionary)
        case LengthOf(FieldExpr(field)) => LengthExpr(expr, field)
        case FieldPathWithOp(path, op) => PathExpr(expr, path, op)
        case s if dictionary.isDefined => PrimitiveWithDictionaryFallbackExpr(s, dictionary.get)
        case s => PrimitiveExpr(s)
      })
    }

    /**
     * Unapply or die.
     */
    def expressionFor(expr: String): FieldExpr = unapply(expr).get

    def expressionsFor(exprList: String): Seq[FieldExpr] = expressionsFor(exprList.split(","))

    def allOf(exprList: String): AllOfExpr = AllOfExpr(exprList, expressionsFor(exprList))

    def expressionsFor(exprList: Seq[String]): Seq[FieldExpr] = {
      exprList.map { case FieldExpr(e) => e }
    }
  }

  object DirectExpr {
    def unapply(expr: String): Option[DirectExpr] = FieldExpr.unapply(expr).flatMap {
      case e: DirectExpr => Some(e)
      case _ => None
    }
  }

  object AggregateOp {

    import io.github.binaryfoo.lagotto.AggregateOps._
    import logFilterParser.LogFilter

    /**
     * Unapply or die.
     */
    def operationFor(expr: String): AggregateOp = unapply(expr).get

    def unapply(expr: String): Option[AggregateOp] = {
      val op: AggregateOp = expr match {
        case "count" => new CountBuilder
        case CountDistinct(DirectExpr(field)) => new CountDistinctBuilder(field)
        case CountIf(LogFilter(condition)) => new CountIfBuilder(condition)
        case MinOp(DirectExpr(field)) => new TryLongOpBuilder(field, minLong, minString)
        case MaxOp(DirectExpr(field)) => new TryLongOpBuilder(field, maxLong, maxString)
        case SumOp(DirectExpr(field)) => new LongOpBuilder(field, addLongs)
        case AvgOp(DirectExpr(field)) => new AverageBuilder(field)
        case GroupConcatDistinct(DirectExpr(field)) => new GroupConcatDistinctBuilder(field)
        case GroupConcat(DirectExpr(field)) => new GroupConcatBuilder(field)
        case GroupSample(DirectExpr(field), size) => new GroupSampleBuilder(field, size.toInt)
        case GroupTrace(filePrefix) => new GroupTraceBuilder(filePrefix)
        case _ => null
      }
      Option(op)
    }

  }
  
  implicit def stringAsFieldAccessor[T <: LogEntry](s: String): FieldAccessor[T] = { e: T => FieldExpr.expressionFor(s)(e) }
}

object RenderHint extends Enumeration {
  type RenderHint = Value
  val Html, RichText = Value
}

/**
 * Exists separately from FieldAccessor to allow passing lambdas to LogEntry.toXsv(). Maybe misguided.
 */
trait FieldExpr extends FieldAccessor[LogEntry] {
  /**
   * The source text of this expression. This name is confusing. Maybe expr or sourceText would be better.
   */
  def field: String
  def contains(other: FieldExpr): Boolean = this == other
  def get(e: LogEntry): Option[String] = Option(apply(e))
  override def toString(): String = field
}

/**
 * Just access a field. The simplest case.
 */
case class PrimitiveExpr(field: String) extends DirectExpr {
  def apply(e: LogEntry): String = e(field)
}

/**
 * Try to access the field but if it doesn't exist check if field is actually a name in the dictionary and try again.
 *
 * The name lookup is deferred and performed for each individual log entry since a given name can be bound to different
 * paths based on some combination of realm, mti, nmic, etc. Eg Some message format might have privateThing as 48.1
 * some messages and 48.48 in others.
 */
case class PrimitiveWithDictionaryFallbackExpr(field: String, dictionary: DataDictionary) extends DirectExpr {
  def apply(e: LogEntry): String = {
    val v = e(field)
    if (v == null && field != "mti") {
      dictionary.fieldForShortName(field, e).map(e(_)).orNull
    } else {
      v
    }
  }
}

/**
 * The delay between a message and the one preceding it (according to the prevailing sort order).
 * Requires a call to io.github.binaryfoo.lagotto.DelayTimer#calculateDelays(scala.collection.immutable.Stream).
 */
object DelayExpr extends DirectExpr {
  val field = "delay"

  def apply(e: LogEntry): String = {
    if (!e.isInstanceOf[DelayTimer]) {
      throw new IllegalStateException(s"We lost delay calculation. Can't retrieve delay from $e")
    }
    e(field)
  }

  def calculateDelays(s: Iterator[LogEntry]): Iterator[DelayTimer] = {
    var previous: Option[LogEntry] = None
    s.map { e =>
      val next = DelayTimer(e, previous)
      previous = Some(e)
      next
    }
  }
}

case class ElapsedExpr() extends DirectExpr {
  private var first = 0l
  val field = "elapsed"
  override def apply(e: LogEntry): String = {
    (if (first == 0) {
      first = e.timestamp.getMillis
      0
    } else {
      e.timestamp.getMillis - first
    }).toString
  }
}

case class ElapsedSinceExpr(field: String, condition: LogFilter) extends DirectExpr {
  private var previousMatch = 0l
  override def apply(e: LogEntry): String = {
    val result = (if (previousMatch == 0) {
      0
    } else {
      e.timestamp.getMillis - previousMatch
    }).toString
    if (condition(e))
      previousMatch = e.timestamp.getMillis
    result
  }
}

/**
 * A marker to indicate an expression does not require the calculation of any aggregate expression.
 *
 * However the result of a DirectExpr can in turn be aggregated. For example in max(calc(timestamp-lifespan))
 * calc(timestamp-lifespan) becomes a DirectExpr (SubtractDirectMillisFromTimeExpr) which is wrapped by an
 * AggregateExpr to find the max() of this calculation.
 *
 * The value can be obtained without running aggregation but unlike a PrimitiveExpr a DirectExpr may require a
 * calculation combining one or more fields from a single log entry. Eg subtraction or division.
 *
 * A thing defined by being the opposite of an aggregate expression.
 */
trait DirectExpr extends FieldExpr

/**
 * An expression that requires an aggregation operation to be performed in order to retrieve the value.
 * @param field The expression to be calculated. Used as a lookup key for the aggregate value.
 * @param op How to calculate the aggregate value.
 */
case class AggregateExpr(field: String, op: AggregateOp) extends FieldExpr {
  override def apply(e: LogEntry): String = {
    if (!e.isInstanceOf[AggregateLogEntry]) {
      throw new IllegalStateException(s"We lost aggregation. Can't retrieve $field from $e")
    }
    e(field)
  }

  /**
   * The field being aggregated. Optional because count doesn't have to act on a single field.
   */
  def expr: Option[FieldExpr] = {
    op match {
      case e: FieldBasedAggregateOp => Some(e.expr)
      case CountIfBuilder(FieldFilterOn(expr)) => Some(expr)
      case _ => None
    }
  }

  override def contains(other: FieldExpr) = expr.exists(e => e.contains(other))
}

object AggregateExpr {

  /**
   * Calculate a set of aggregate values for each set of rows identified the keyFields.
   * @param s The stream of log entries that will be output.
   * @param keyFields The set of fields identifying each group.
   * @param aggregateFields The set of aggregates to calculate for each group.
   * @return An Iterator[AggregateLogEntry].
   */
  def aggregate(s: Iterator[LogEntry], keyFields: Seq[FieldExpr], aggregateFields: Seq[AggregateExpr]): Iterator[AggregateLogEntry] = {
    def keyFor(e: LogEntry): Seq[(String, String)] = {
      for {
        k <- keyFields
      } yield (k.field, k(e))
    }
    def newBuilder(k: Seq[(String, String)]) = {
      // Each aggregate holds state so needs to be cloned for each new group
      val aggregates = aggregateFields.map(e => (e.field, e.op.copy()))
      new AggregateLogEntryBuilder(k.toMap, aggregates)
    }
    OrderedGroupBy.groupByOrdered(s, keyFor, newBuilder).iterator
  }
}

object HasAggregateExpressions {
  def unapply(expr: FieldExpr): Option[Seq[AggregateExpr]] = {
    expr match {
      case e: AggregateExpr => Some(Seq(e))
      case e: CalculationOverAggregates => Some(e.dependencies())
      case AliasExpr(t: AggregateExpr, _) => Some(Seq(t))
      case AliasExpr(t: CalculationOverAggregates, _) => Some(t.dependencies())
      case _ => None
    }
  }
}

/**
 * Handle a function of one or more fields (calculation) where the fields are aggregates (a function of one or more rows).
 *
 * Marks an expression as dependencies on the output of aggregation. Such as expression can't be evaluated without
 * first calculating the dependencies.
 *
 * An aggregate of an aggregate is not currently a thing (not permitted).
 */
trait CalculationOverAggregates extends FieldExpr {

  final def apply(e: LogEntry): String = calculate(e)

  /**
   * Only exists to allow expression implementations to share code between the direct and aggregate versions.
   * Maybe there's a better way.
   */
  def calculate(e: LogEntry): String

  /**
   * The aggregate expressions involved in this calculation.
   */
  def dependencies(): Seq[AggregateExpr]
}

/**
 * Handles two cases:
 *   1. A function of one or more fields (calculation).
 *   2. An aggregate of a calculation.
 *
 * In the latter the aggregation process does the calculation.
 * Trying to perform the calculation on the output of aggregation would fail because the underlying fields are gone.
 * Unless the calculation is happening over aggregated fields.
 */
trait DirectCalculationExpr extends DirectExpr {
  final def apply(e: LogEntry): String = {
    e match {
      case aggregated: AggregateLogEntry => e(field)
      case _ => calculate(e)
    }
  }
  def calculate(e: LogEntry): String
}

object SubtractExpr {

  def apply(expr: String, l: FieldExpr, r: FieldExpr): FieldExpr = {
    (l, r) match {
      case (left: AggregateExpr, right: AggregateExpr) =>
        left.field match {
          case AggregateOps.OverExpression(TimeFormatter(leftFormat)) => right.field match {
            case AggregateOps.OverExpression(TimeFormatter(rightFormat)) => SubtractTwoAggregateTimesExpr(expr, left, right, leftFormat, rightFormat)
            case _ => SubtractAggregateMillisFromTimeExpr(expr, left, right, leftFormat)
          }
          case _ => SubtractAggregatesExpr(expr, left, right)
        }
      case (left: DirectExpr, right: DirectExpr) =>
        left.field match {
          case TimeFormatter(leftFormat) => right.field match {
            case TimeFormatter(rightFormat) => SubtractTwoDirectTimesExpr(expr, left, right, leftFormat, rightFormat)
            case _ => SubtractDirectMillisFromTimeExpr(expr, left, right, leftFormat)
          }
          case _ => SubtractDirectExpr(expr, left, right)
        }
      case (left, right) =>
        throw new IAmSorryDave(s"In calc(left-right) both sides must be aggregate or direct operations. Left: $left and Right: $right are not compatible.")
    }
  }
}

/**
 * Show the difference between two timestamps as a period.
 */
trait SubtractTimestampsExpr {

  def left: FieldExpr
  def right: FieldExpr
  def leftFormat: TimeFormatter
  def rightFormat: TimeFormatter

  def calculate(e: LogEntry): String = {
    val leftTime = leftFormat.parseDateTime(left(e))
    val rightTime = rightFormat.parseDateTime(right(e))
    val period = new Period(rightTime, leftTime)
    leftFormat.print(period)
  }
}

case class SubtractTwoAggregateTimesExpr(field: String, left: AggregateExpr, right: AggregateExpr, leftFormat: TimeFormatter, rightFormat: TimeFormatter)
  extends SubtractTimestampsExpr with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateExpr] = Seq(left, right)
}

case class SubtractTwoDirectTimesExpr(field: String, left: DirectExpr, right: DirectExpr, leftFormat: TimeFormatter, rightFormat: TimeFormatter)
  extends SubtractTimestampsExpr with DirectCalculationExpr {
}

/**
 * Subtract two values convertible to Longs.
 */
trait SubtractExpr {

  def left: FieldExpr
  def right: FieldExpr

  def calculate(e: LogEntry): String = {
    (left.get(e), right.get(e)) match {
      case (Some(l), Some(r)) => (l.toLong - r.toLong).toString
      case _ => null
    }
  }
}

case class SubtractAggregatesExpr(field: String, left: AggregateExpr, right: AggregateExpr)
  extends SubtractExpr with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateExpr] = Seq(left, right)
}

case class SubtractDirectExpr(field: String, left: DirectExpr, right: DirectExpr)
  extends SubtractExpr with DirectCalculationExpr {
}

/**
 * Show a new timestamp that a number of milliseconds (right) prior to the original (left).
 */
trait SubtractMillisFromTimeExpr {

  def left: FieldExpr
  def right: FieldExpr
  def leftFormat: TimeFormatter

  def calculate(e: LogEntry): String = {
    val leftValue = left(e)
    val rightValue = right(e)
    if (leftValue == null || rightValue == null) {
      null
    }
    else {
      val leftTime = leftFormat.parseDateTime(leftValue)
      leftFormat.print(leftTime.minusMillis(rightValue.toInt))
    }
  }

}

case class SubtractAggregateMillisFromTimeExpr(field: String, left: AggregateExpr, right: AggregateExpr, leftFormat: TimeFormatter)
  extends SubtractMillisFromTimeExpr with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateExpr] = Seq(left, right)
}

case class SubtractDirectMillisFromTimeExpr(field: String, left: DirectExpr, right: DirectExpr, leftFormat: TimeFormatter)
  extends SubtractMillisFromTimeExpr with DirectCalculationExpr {
}

object DivideExpr {
  def apply(expr: String, l: FieldExpr, r: FieldExpr): FieldExpr = {
    (l, r) match {
      case (left: AggregateExpr, right: AggregateExpr) => DivideAggregatesExpr(expr, left, right)
      case (left: DirectExpr, right: DirectExpr) => DivideDirectExpr(expr, left, right)
      case (left, right) =>
        throw new IAmSorryDave(s"In calc(left/right) both sides must be aggregate or direct operations. Left: $left and Right: $right are not compatible.")
    }
  }
}

/**
 * Only use case might be calculating a percentage.
 */
trait DivideExpr {
  def left: FieldExpr
  def right: FieldExpr

  def calculate(e: LogEntry): String = {
    val leftNumber = left(e).toDouble
    val rightNumber = right(e).toDouble
    (leftNumber / rightNumber).formatted("%.4f")
  }
}

case class DivideAggregatesExpr(field: String, left: AggregateExpr, right: AggregateExpr)
  extends DivideExpr with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateExpr] = Seq(left, right)
}

case class DivideDirectExpr(field: String, left: DirectExpr, right: DirectExpr)
  extends DivideExpr with DirectCalculationExpr {
}

/**
 * A limited set of type conversions.
 */
trait ConvertExpr {

  def expr: FieldExpr
  def op: ConvertExpr.ConversionOp
  def contains(other: FieldExpr): Boolean = expr.contains(other)

  def calculate(e: LogEntry): String = {
    val value = expr(e)
    if (value == null || value == "") {
      null
    } else {
      op.apply(value)
    }
  }
}

object ConvertExpr {
  type ConversionOp = String => String
  type TimeConversionOp = (String, TimeFormatter, TimeFormatter) => String

  def apply(field: String, expr: FieldExpr, from: String, to: String): FieldExpr = {
    val op: ConversionOp = (expr.field, from, to) match {
      case (_, "millis", "period") => TimeConversion(millisToPeriod, null, DefaultDateTimeFormat)
      case (_, "millis", TimeFormatter(f)) => TimeConversion(millisToPeriod, null, f)
      case (_, "ms", TimeFormatter(f)) => TimeConversion(millisToPeriod, null, f)
      case (_, null, TimeFormatter(f)) => TimeConversion(millisToPeriod, null, f)
      case (TimeFormatter(inputFormat), null, "millis") => TimeConversion(timeToMillisOfDay, inputFormat, null)
      case (TimeFormatter(inputFormat), null, "ms") => TimeConversion(timeToMillisOfDay, inputFormat, null)
      case (_, "time", TimeFormatter(outputFormat)) => TimeConversion(timeToPeriod, DefaultTimeFormat, outputFormat)
      case (_, "time", "millis") => TimeConversion(timeToMillisOfDay, DefaultDateTimeFormat, null)
      case (_, "time", "ms") => TimeConversion(timeToMillisOfDay, DefaultDateTimeFormat, null)
      case (_, "micro", "seconds") => microToSeconds
      case (_, "us", "s") => microToSeconds
      case (_, "micro", "millis") => microToMillis
      case (_, "us", "ms") => microToMillis
      case (_, "millis", "seconds") => millisToSeconds
      case (_, "ms", "s") => millisToSeconds
      case (_, null, "int") => stripZeroes
      case (_, null, "href") => href
      case _ => throw new IAmSorryDave(s"Unknown conversion $field")
    }
    expr match {
      case e: AggregateExpr => ConvertAggregateExpr(field, e, op)
      case e: DirectExpr => ConvertDirectExpr(field, e, op)
    }
  }

  private val oneDpFormat = new DecimalFormat("0.#")

  val millisToPeriod    = (v: String, input: TimeFormatter, output: TimeFormatter) => output.print(new Period(v.toLong))
  val timeToMillisOfDay = (v: String, input: TimeFormatter, output: TimeFormatter) => input.parseDateTime(v).getMillisOfDay.toString
  val timeToPeriod      = (v: String, input: TimeFormatter, output: TimeFormatter) => output.print(input.parseDateTime(v))
  val microToSeconds    = (v: String) => oneDpFormat.format(v.toDouble / 1000000)
  val microToMillis     = (v: String) => (v.toLong / 1000).toString
  val millisToSeconds   = (v: String) => oneDpFormat.format(v.toDouble / 1000)
  val stripZeroes       = (v: String) => v.toInt.toString
  val href              = (v: String) => SourceHrefExpr.linkTo(v, v)
}

private case class TimeConversion(op: TimeConversionOp, in: TimeFormatter, out: TimeFormatter) extends ConvertExpr.ConversionOp {
  override def apply(v: String): String = op(v, in, out)
}

case class ConvertAggregateExpr(field: String, expr: AggregateExpr, op: ConvertExpr.ConversionOp)
  extends ConvertExpr with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateExpr] = Seq(expr)
  override def contains(other: FieldExpr): Boolean = super[ConvertExpr].contains(other)
}

case class ConvertDirectExpr(field: String, expr: DirectExpr, op: ConvertExpr.ConversionOp)
  extends ConvertExpr with DirectCalculationExpr {
  override def contains(other: FieldExpr): Boolean = super[ConvertExpr].contains(other)
}

/**
 * Perform a dictionary lookup to convert a value into English (or something else).
 * Currently only works on direct not on an aggregation result.
 */
case class TranslateExpr(field: String, path: String, dictionary: DataDictionary) extends DirectCalculationExpr {

  override def calculate(e: LogEntry): String = {
    val value = e(path)
    (if (value == null) {
      dictionary.fieldForShortName(path, e).map(f => (f, e(f)))
    } else {
      Some((path, value))
    }).flatMap { case (f, v) =>
      dictionary.translateValue(f, e, v)
    }.orNull
  }

}

/**
 * Find lines in a field of a log entry that match a pattern. Replace the match with replacement.
 */
case class RegexReplaceExpr(field: String, expr: DirectExpr, pattern: String, replacement: String) extends DirectCalculationExpr {

  val regex = Pattern.compile(pattern)

  override def calculate(e: LogEntry): String = {
    val raw = expr(e)
    if (raw != null) {
      val lines = raw.split('\n')
      if (lines.length > 1) {
        lines.flatMap { line =>
          val m = regex.matcher(line)
          if (m.find()) {
            Some(m.replaceAll(replacement))
          } else {
            None
          }
        }.mkString("\n")
      } else {
        regex.matcher(lines.head).replaceAll(replacement)
      }
    } else {
      null
    }
  }

}

case class PivotExpr(field: String, pivot: FieldExpr) extends DirectExpr {

  private val values = new mutable.LinkedHashSet[String]()

  def apply(e: LogEntry): String = {
    val value = pivot(e)
    values.add(value)
    value
  }

  def distinctValues(): Seq[String] = values.toSeq
}

case class PivotResultExpr(field: String, pivotedField: String) extends DirectExpr {
  override def apply(e: LogEntry): String = e(pivotedField)
}

/**
 * Handles 48.max, 48.min and 48.*.
 */
case class PathExpr(field: String, path: Seq[String], op: String) extends DirectExpr {

  private val (left, Seq(_, right @ _*)) = path.splitAt(path.indexOf(op))

  override def apply(e: LogEntry): String = {
    (e match {
      case JposEntry(fields, _, _) if op == "*" => Some(pathsWithPrefix(fields).map(e(_)).mkString(","))
      case JposEntry(fields, _, _) if op == "min" => nextItemsAfterPrefix(fields).headOption.map(u => concat(e, u))
      case JposEntry(fields, _, _) => nextItemsAfterPrefix(fields).lastOption.map(u => concat(e, u))
      case _ => None
    }).getOrElse(e(field))
  }

  private def nextItemsAfterPrefix(fields: mutable.Map[String, String]): Seq[String] = {
    fields.keySet.collect {
      case FieldPath(k) if k.startsWith(left) && k.length > left.length => k(left.length)
    }.toSeq.sorted
  }

  private def pathsWithPrefix(fields: mutable.Map[String, String]): Seq[String] = {
    fields.keySet.collect {
      case FieldPath(k) if k.startsWith(left) && k.length > left.length => FieldPath(k)
    }.toSeq.sorted
  }

  private def concat(e: LogEntry, u: String): String = e(FieldPath((left :+ u) ++ right))

}

case class XPathExpr(field: String, xpath: String) extends DirectExpr {

  private val expr = XPathEval.compile(xpath)

  override def apply(e: LogEntry): String = expr(e.lines)
}

case class TimeExpr(field: String, formatter: TimeFormatter) extends DirectCalculationExpr {
  override def calculate(e: LogEntry): String = {
    e match {
      case p: MsgPair => e(field)
      case _ => formatter.print(e.timestamp)
    }
  }
}

object TimeExpr {
  def unapply(expr: String): Option[TimeExpr] = {
    TimeFormatter.unapply(expr).map(f => TimeExpr(expr, f))
  }
}

case class AllOfExpr(field: String, expressions: Seq[FieldExpr]) extends FieldExpr {
  override def apply(e: LogEntry): String = expressions.map(_(e)).mkString(",")
}

case class AliasExpr(target: FieldExpr, name: String) extends FieldExpr {
  override def field: String = target.field
  override def apply(e: LogEntry): String = target(e)
  override def contains(other: FieldExpr): Boolean = target.contains(other)
  override def toString(): String = name
}

/**
 * Where the log entry was loaded from as a hyperlink.
 */
object SourceHrefExpr extends DirectExpr {
  override def field: String = "src"

  override def apply(e: LogEntry): String = {
    e.source match {
      case r@FileRef(file, line) => linkTo(urlFor(file, line, e), r.toString)
      case r => r.toString
    }
  }

  def linkTo(url: String, mouseOver: String): String = s"""<a href="$url" title="$mouseOver">&#9906;</a>"""

  def urlFor(file: File, line: Int, e: LogEntry) = {
    val to = (e match {
      case MsgPair(req, resp) => resp.source.line + resp.lines.split('\n').size
      case JoinedEntry(left, right, _, _) => right.source.line + right.lines.split('\n').size
      case _ => line + e.lines.split('\n').size
    }) - 1
    file.toURI.toURL.toString.substring(5) + s"?from=${line-1}&to=$to"
  }
}

object SourceExpr extends DirectExpr {
  override def field: String = "src"
  override def apply(e: LogEntry): String = e.source.toString
}

object FileExpr extends DirectExpr {
  override def field: String = "file"
  override def apply(e: LogEntry): String = e.source.name
}

object LineExpr extends DirectExpr {
  override def field: String = "line"
  override def apply(e: LogEntry): String = e.source.line.toString
}

object LinesExpr extends DirectExpr {
  override def field: String = "lines"
  override def apply(e: LogEntry): String = e.lines
}

abstract class IconExpr extends DirectExpr {
  val send: String
  val receive: String
  val sessionStart: String
  val sessionEnd: String
  val sessionError: String
  val peerDisconnect: String
  val ioTimeout: String

  final override def field: String = "icon"
  final override def apply(e: LogEntry): String = e match {
    case j: JposEntry => j.msgType match {
      case "send" => send
      case "receive" => receive
      case "session-start" => sessionStart
      case "session-end" => sessionEnd
      case "session-error" => sessionError
      case "peer-disconnect" => peerDisconnect
      case "io-timeout" => ioTimeout
      case _ if j.fields.contains("exception") => sessionError
      case t => t
    }
    case _ => e("icon")
  }
}

object UnicodeIconExpr extends IconExpr {
  override val send = "\u2192"
  override val receive = "\u2190"
  override val sessionStart = "\u21a6"
  override val sessionEnd = "\u2717"
  override val sessionError = "\u2620"
  override val peerDisconnect = "\u2604"
  override val ioTimeout = "\u23f0"
}

object AsciiIconExpr extends IconExpr {
  override val send = "->"
  override val receive = "<-"
  override val sessionStart = "[~"
  override val sessionEnd = "~]"
  override val sessionError = "!!"
  override val peerDisconnect = "X"
  override val ioTimeout = "T"
}

object HtmlIconExpr extends DirectExpr {
  override def field: String = "icon"
  override def apply(e: LogEntry): String = e match {
    case j: JposEntry =>
      val icon = UnicodeIconExpr(e)
      if (icon != null) "&#" + icon.codePointAt(0) + ";"
      else icon
    case _ => e("icon")
  }
}

case class SummaryExpr(icon: FieldExpr, dictionary: Option[DataDictionary]) extends DirectExpr {
  override def field: String = "summary"
  override def apply(e: LogEntry): String = e match {
    case j: JposEntry if j.contains("0") && j.contains("70") =>
      val mti = j("0")
      val nmic = j("70")
      val s = icon(j) + " " + mti + " " + nmic
      dictionary.map(d => s + " (" + d.translateValue("0", e, mti).getOrElse("?") + " " + d.translateValue("70", e, nmic).getOrElse("?") + ")").getOrElse(s)
    case j: JposEntry if j.contains("0") =>
      val mti = j("0")
      val s = icon(j) + " " + mti
      dictionary.map(d => s + " (" + d.translateValue("0", e, mti).getOrElse("?") + ")").getOrElse(s)
    case j: JposEntry if j.contains("exception") =>
      icon(j) + " " + j("exception")
    case j: JposEntry =>
      icon(j)
    case e: Log4jEntry if e.nested.isDefined =>
      apply(e.nested.get)
    case e: Log4jEntry =>
      e.message
    case DelayTimer(timed, _) =>
      apply(timed)
    case e: AggregateLogEntry =>
      e(field)
    case _ if e.lines != null =>
      e.lines.split('\n')(0)
  }
}

case class LengthExpr(field: String, of: FieldExpr) extends DirectExpr {
  override def apply(e: LogEntry): String = {
    val value = of(e)
    if (value != null) value.length.toString else null
  }
}