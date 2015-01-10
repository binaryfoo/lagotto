package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.dictionary.DataDictionary
import org.joda.time.Period

object FieldExpr {

  // Not sure how to avoid this global state. Implicit parameter?
  var dictionary: Option[DataDictionary] = None

  val SubtractOp = """calc\((.+)-(.+)\)""".r
  val DivideOp = """calc\((.+)/(.+)\)""".r
  val ConvertOp = """\(([^ ]+) (?:(.+) )?as (.+)\)""".r
  val TranslateOp = """translate\((.+)\)""".r

  def unapply(expr: String): Option[FieldExpr] = {
    Some(expr match {
      case SubtractOp(FieldExpr(left), FieldExpr(right)) => SubtractExpr(expr, left, right)
      case DivideOp(FieldExpr(left), FieldExpr(right)) => DivideExpr(expr, left, right)
      case ConvertOp(FieldExpr(child), from, to) => ConvertExpr(expr, child, from, to)
      case "delay" => DelayExpr
      case AggregateOp(op) => AggregateExpr(expr, op)
      case TranslateOp(field) => TranslateExpr(expr, field, dictionary.getOrElse(throw new IAmSorryDave(s"No dictionary configured. Can't translate '$expr'")))
      case s if dictionary.isDefined => PrimitiveWithDictionaryFallbackExpr(s, dictionary.get)
      case s => PrimitiveExpr(s)
    })
  }

  /**
   * Unapply or die.
   */
  def expressionFor(expr: String): FieldExpr = unapply(expr).get

  def expressionsFor(exprList: String): Seq[FieldExpr] = expressionsFor(exprList.split(","))

  def expressionsFor(exprList: Seq[String]): Seq[FieldExpr] = {
    exprList.map { case FieldExpr(e) => e }
  }
}

/**
 * Exists separately from FieldAccessor to allow passing lambdas to LogLike.toXsv(). Maybe misguided.
 */
trait FieldExpr extends FieldAccessor[LogLike] {
  /**
   * The source text of this expression.
   */
  def field: String
  override def toString(): String = field
}

/**
 * Just access a field. The simplest case.
 */
case class PrimitiveExpr(field: String) extends DirectExpr {
  def apply(e: LogLike): String = e(field)
}

/**
 * Try to access the field but if it doesn't exist check if field is actually a name in the dictionary and try again.
 *
 * The name lookup is deferred and performed for each individual log entry since a given name can be bound to different
 * paths based on some combination of realm, mti, nmic, etc. Eg Some message format might have privateThing as 48.1
 * some messages and 48.48 in others.
 */
case class PrimitiveWithDictionaryFallbackExpr(field: String, dictionary: DataDictionary) extends DirectExpr {
  def apply(e: LogLike): String = {
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

  def apply(e: LogLike): String = {
    if (!e.isInstanceOf[DelayTimer]) {
      throw new IllegalStateException(s"We lost delay calculation. Can't retrieve delay from $e")
    }
    e(field)
  }

  def calculateDelays(s: Iterator[LogLike]): Iterator[DelayTimer] = {
    var previous: Option[LogLike] = None
    s.map { e =>
      val next = DelayTimer(e, previous)
      previous = Some(e)
      next
    }
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

object DirectExpr {
  def unapply(expr: String): Option[DirectExpr] = FieldExpr.unapply(expr).flatMap {
    case e: DirectExpr => Some(e)
    case _ => None
  }
}

/**
 * An expression that requires an aggregation operation to be performed in order to retrieve the value.
 * @param field The expression to be calculated. Used as a lookup key for the aggregate value.
 * @param op How to calculate the aggregate value.
 */
case class AggregateExpr(field: String, op: AggregateOp) extends FieldExpr {
  override def apply(e: LogLike): String = {
    if (!e.isInstanceOf[AggregateLogLike]) {
      throw new IllegalStateException(s"We lost aggregation. Can't retrieve $field from $e")
    }
    e(field)
  }
}

object AggregateExpr {

  def unapply(expr: String): Option[AggregateExpr] = expr match {
    case AggregateOp(op) => Some(AggregateExpr(expr, op))
    case _ => None
  }

  /**
   * Calculate a set of aggregate values for each set of rows identified the keyFields.
   * @param s The stream of log entries that will be output.
   * @param keyFields The set of fields identifying each group.
   * @param aggregateFields The set of aggregates to calculate for each group.
   * @return An Iterator[AggregateLogLike].
   */
  def aggregate(s: Iterator[LogLike], keyFields: Seq[FieldExpr], aggregateFields: Seq[AggregateExpr]): Iterator[AggregateLogLike] = {
    def keyFor(e: LogLike): Seq[(String, String)] = {
      for {
        k <- keyFields
      } yield (k.field, k(e))
    }
    def newBuilder(k: Seq[(String, String)]) = {
      // Each aggregate holds state so needs to be cloned for each new group
      val aggregates = aggregateFields.map(e => (e.field, e.op.copy()))
      new AggregateLogLikeBuilder(k.toMap, aggregates)
    }
    OrderedGroupBy.groupByOrdered(s, keyFor, newBuilder).values.iterator
  }
}

object HasAggregateExpressions {
  def unapply(expr: FieldExpr): Option[Seq[AggregateExpr]] = {
    expr match {
      case e: AggregateExpr => Some(Seq(e))
      case e: CalculationOverAggregates => Some(e.dependencies())
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

  final def apply(e: LogLike): String = calculate(e)

  /**
   * Only exists to allow expression implementations to share code between the direct and aggregate versions.
   * Maybe there's a better way.
   */
  def calculate(e: LogLike): String

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
  final def apply(e: LogLike): String = {
    e match {
      case aggregated: AggregateLogLike => e(field)
      case _ => calculate(e)
    }
  }
  def calculate(e: LogLike): String
}

object SubtractExpr {
  def apply(expr: String, l: FieldExpr, r: FieldExpr): FieldExpr = {
    (l, r) match {
      case (left: AggregateExpr, right: AggregateExpr) =>
        val leftFormat = left.field match {
          case AggregateOp.OverExpression(TimeFormatter(format)) => format
          case _ => throw new IAmSorryDave(s"In calc(left-right) left must be time expression. ${left.field} is not a time expression.")
        }
        right.field match {
          case AggregateOp.OverExpression(TimeFormatter(rightFormat)) => SubtractTwoAggregateTimesExpr(expr, left, right, leftFormat, rightFormat)
          case _ => SubtractAggregateMillisFromTimeExpr(expr, left, right, leftFormat)
        }
      case (left: DirectExpr, right: DirectExpr) =>
        val leftFormat = left.field match {
          case TimeFormatter(format) => format
          case _ => throw new IAmSorryDave(s"In calc(left-right) left must be time expression. ${left.field} is not a time expression.")
        }
        right.field match {
          case TimeFormatter(rightFormat) => SubtractTwoDirectTimesExpr(expr, left, right, leftFormat, rightFormat)
          case _ => SubtractDirectMillisFromTimeExpr(expr, left, right, leftFormat)
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

  def calculate(e: LogLike): String = {
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
 * Show a new timestamp that a number of milliseconds (right) prior to the original (left).
 */
trait SubtractMillisFromTimeExpr {

  def left: FieldExpr
  def right: FieldExpr
  def leftFormat: TimeFormatter

  def calculate(e: LogLike): String = {
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

  def calculate(e: LogLike): String = {
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
  def op: ConvertExpr.TimeConversionOp
  def input: TimeFormatter
  def output: TimeFormatter

  def calculate(e: LogLike): String = {
    val value = expr(e)
    if (value == null || value == "") {
      null
    } else {
      op.apply(value, input, output)
    }
  }
}

object ConvertExpr {
  type TimeConversionOp = (String, TimeFormatter, TimeFormatter) => String

  def apply(field: String, expr: FieldExpr, from: String, to: String): FieldExpr = {
    val (op: TimeConversionOp, input: TimeFormatter, output: TimeFormatter) = (expr.field, from, to) match {
      case (_, "millis", "period") => (millisToPeriod, DefaultDateTimeFormat, DefaultDateTimeFormat)
      case (_, "millis", TimeFormatter(f)) => (millisToPeriod, f, f)
      case (_, null, TimeFormatter(f)) => (millisToPeriod, f, f)
      case (TimeFormatter(inputFormat), null, "millis") => (timeToMillisOfDay, inputFormat, inputFormat)
      case (_, "time", TimeFormatter(outputFormat)) => (timeToPeriod, DefaultTimeFormat, outputFormat)
      case (_, "time", "millis") => (timeToMillisOfDay, DefaultDateTimeFormat, DefaultDateTimeFormat)
      case _ => throw new IAmSorryDave(s"Unknown conversion $field")
    }
    expr match {
      case e: AggregateExpr => ConvertAggregateExpr(field, e, op, input, output)
      case e: DirectExpr => ConvertDirectExpr(field, e, op, input, output)
    }
  }

  val millisToPeriod    = (v: String, input: TimeFormatter, output: TimeFormatter) => output.print(new Period(v.toLong))
  val timeToMillisOfDay = (v: String, input: TimeFormatter, output: TimeFormatter) => input.parseDateTime(v).getMillisOfDay.toString
  val timeToPeriod      = (v: String, input: TimeFormatter, output: TimeFormatter) => output.print(input.parseDateTime(v))
}

case class ConvertAggregateExpr(field: String, expr: AggregateExpr, op: ConvertExpr.TimeConversionOp, input: TimeFormatter, output: TimeFormatter)
  extends ConvertExpr with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateExpr] = Seq(expr)
}

case class ConvertDirectExpr(field: String, expr: DirectExpr, op: ConvertExpr.TimeConversionOp, input: TimeFormatter, output: TimeFormatter)
  extends ConvertExpr with DirectCalculationExpr {
}

/**
 * Perform a dictionary lookup to convert a value into English (or something else).
 * Currently only works on direct not on an aggregation result.
 */
case class TranslateExpr(field: String, raw: String, dictionary: DataDictionary) extends DirectCalculationExpr {

  override def calculate(e: LogLike): String = {
    val value = e(raw)
    dictionary.translateValue(raw, e, value).getOrElse(value)
  }

}