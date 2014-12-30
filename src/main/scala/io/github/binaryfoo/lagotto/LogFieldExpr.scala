package io.github.binaryfoo.lagotto

import org.joda.time.{DateTimeZone, Period}

object LogFieldExpr {
  val SubtractOp = """calc\((.+)-(.+)\)""".r
  val DivideOp = """calc\((.+)/(.+)\)""".r
  val ConvertOp = """\(([^ ]+) (?:(.+) )?as (.+)\)""".r

  def unapply(expr: String): Option[GroundedFieldExpr] = {
    Some(expr match {
      case SubtractOp(LogFieldExpr(left), LogFieldExpr(right)) => SubtractExpr(expr, left, right)
      case DivideOp(LogFieldExpr(left), LogFieldExpr(right)) => DivideExpr(expr, left, right)
      case ConvertOp(LogFieldExpr(child), from, to) => ConvertExpr(expr, child, from, to)
      case "delay" => DelayFieldExpr
      case AggregateOp(op) => AggregateFieldExpr(expr, op)
      case s => PrimitiveLogFieldExpr(s)
    })
  }

  /**
   * Unapply or die.
   */
  def expressionFor(expr: String): GroundedFieldExpr = unapply(expr).get

  def expressionsFor(exprList: String): Seq[GroundedFieldExpr] = expressionsFor(exprList.split(","))

  def expressionsFor(exprList: Seq[String]): Seq[GroundedFieldExpr] = {
    exprList.map { case LogFieldExpr(e) => e }
  }
}

object AggregateFieldExpr {
  def unapply(expr: String): Option[AggregateFieldExpr] = expr match {
    case AggregateOp(op) => Some(AggregateFieldExpr(expr, op))
    case _ => None
  }
}

object DirectLogFieldExpr {
  def unapply(expr: String): Option[DirectLogFieldExpr] = LogFieldExpr.unapply(expr).flatMap {
    case e: DirectLogFieldExpr => Some(e)
    case _ => None
  }
}

/**
 * Exists separately from LogFieldExpr to allow passing lambas to LogLike.toXsv(). Maybe misguided.
 */
trait GroundedFieldExpr extends LogFieldExpr {
  def field: String
  override def toString(): String = field
}

/**
 * Just access a field.
 */
case class PrimitiveLogFieldExpr(field: String) extends DirectLogFieldExpr {
  def apply(e: LogLike): String = e(field)
}

object DelayFieldExpr extends DirectLogFieldExpr {
  val field = "delay"
  def apply(e: LogLike): String = {
    if (!e.isInstanceOf[DelayTimer]) {
      throw new IllegalStateException(s"We lost delay calculation. Can't retrieve delay from $e")
    }
    e(field)
  }
}

/**
 * The opposite of an aggregate expression. The value can be obtained without running aggregation.
 */
trait DirectLogFieldExpr extends GroundedFieldExpr

/**
 * An expression that requires an aggregation operation to be performed in order to retrieve the value.
 * @param field The expression to be calculated. Used as a lookup key for the aggregate value.
 * @param op How to calculate the aggregate value.
 */
case class AggregateFieldExpr(field: String, op: AggregateOp) extends GroundedFieldExpr {
  override def apply(e: LogLike): String = {
    if (!e.isInstanceOf[AggregateLogLike]) {
      throw new IllegalStateException(s"We lost aggregation. Can't retrieve $field from $e")
    }
    e(field)
  }
}

object HasAggregateExpressions {
  def unapply(expr: GroundedFieldExpr): Option[Seq[AggregateFieldExpr]] = {
    expr match {
      case e: AggregateFieldExpr => Some(Seq(e))
      case e: CalculationOverAggregates => Some(e.dependencies())
      case _ => None
    }
  }
}

trait CalculationOverAggregates extends GroundedFieldExpr {
  final def apply(e: LogLike): String = calculate(e)
  def calculate(e: LogLike): String
  def dependencies(): Seq[AggregateFieldExpr]
}

/**
 * Handles the fact that the aggregation process does the calculation.
 * Trying to perform the calculation on the output of aggregation would fail because the underlying fields are gone.
 * Unless the calculation is happening over aggregated fields.
 */
trait DirectCalculationExpr extends DirectLogFieldExpr {
  final def apply(e: LogLike): String = {
    e match {
      case aggregated: AggregateLogLike => e(field)
      case _ => calculate(e)
    }
  }
  def calculate(e: LogLike): String
}

object SubtractExpr {
  def apply(expr: String, l: GroundedFieldExpr, r: GroundedFieldExpr): GroundedFieldExpr = {
    (l, r) match {
      case (left: AggregateFieldExpr, right: AggregateFieldExpr) =>
        val leftFormat = left.field match {
          case AggregateOp.OverExpression(TimeFormatter(format)) => format
          case _ => throw new IAmSorryDave(s"In calc(left-right) left must be time expression. ${left.field} is not a time expression.")
        }
        right.field match {
          case AggregateOp.OverExpression(TimeFormatter(rightFormat)) => SubtractTwoAggregateTimesExpr(expr, left, right, leftFormat, rightFormat)
          case _ => SubtractAggregateMillisFromTimeExpr(expr, left, right, leftFormat)
        }
      case (left: DirectLogFieldExpr, right: DirectLogFieldExpr) =>
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
trait SubtractTwoTimesOperation {

  def left: GroundedFieldExpr
  def right: GroundedFieldExpr
  def leftFormat: TimeFormatter
  def rightFormat: TimeFormatter

  def calculate(e: LogLike): String = {
    val leftTime = leftFormat.parseDateTime(left(e))
    val rightTime = rightFormat.parseDateTime(right(e))
    val period = new Period(rightTime, leftTime)
    leftFormat.print(period)
  }
}

case class SubtractTwoAggregateTimesExpr(field: String, left: AggregateFieldExpr, right: AggregateFieldExpr, leftFormat: TimeFormatter, rightFormat: TimeFormatter)
  extends SubtractTwoTimesOperation with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateFieldExpr] = Seq(left, right)
}

case class SubtractTwoDirectTimesExpr(field: String, left: DirectLogFieldExpr, right: DirectLogFieldExpr, leftFormat: TimeFormatter, rightFormat: TimeFormatter)
  extends SubtractTwoTimesOperation with DirectCalculationExpr {
}

/**
 * Show a new timestamp that a number of milliseconds (right) prior to the original (left).
 */
trait SubtractMillisFromTimeOperation {

  def left: GroundedFieldExpr
  def right: GroundedFieldExpr
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

case class SubtractAggregateMillisFromTimeExpr(field: String, left: AggregateFieldExpr, right: AggregateFieldExpr, leftFormat: TimeFormatter)
  extends SubtractMillisFromTimeOperation with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateFieldExpr] = Seq(left, right)
}

case class SubtractDirectMillisFromTimeExpr(field: String, left: DirectLogFieldExpr, right: DirectLogFieldExpr, leftFormat: TimeFormatter)
  extends SubtractMillisFromTimeOperation with DirectCalculationExpr {
}

object DivideExpr {
  def apply(expr: String, l: GroundedFieldExpr, r: GroundedFieldExpr): GroundedFieldExpr = {
    (l, r) match {
      case (left: AggregateFieldExpr, right: AggregateFieldExpr) => DivideAggregatesExpr(expr, left, right)
      case (left: DirectLogFieldExpr, right: DirectLogFieldExpr) => DivideDirectExpr(expr, left, right)
      case (left, right) =>
        throw new IAmSorryDave(s"In calc(left/right) both sides must be aggregate or direct operations. Left: $left and Right: $right are not compatible.")
    }
  }
}

trait DivideOperation {
  def left: GroundedFieldExpr
  def right: GroundedFieldExpr

  def calculate(e: LogLike): String = {
    val leftNumber = left(e).toDouble
    val rightNumber = right(e).toDouble
    (leftNumber / rightNumber).formatted("%.4f")
  }
}

case class DivideAggregatesExpr(field: String, left: AggregateFieldExpr, right: AggregateFieldExpr)
  extends DivideOperation with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateFieldExpr] = Seq(left, right)
}

case class DivideDirectExpr(field: String, left: DirectLogFieldExpr, right: DirectLogFieldExpr)
  extends DivideOperation with DirectCalculationExpr {
}

/**
 * A limited set of type conversions.
 */
trait ConvertOperation {

  def expr: GroundedFieldExpr
  def op: ConvertExpr.TimeConversionOp
  def input: TimeFormatter
  def output: TimeFormatter

  def calculate(e: LogLike): String = {
    val value = expr(e)
    if (value == null || value == "") {
      null
    } else {
      val conversion = op
      conversion(value, input, output)
    }
  }
}

object ConvertExpr {
  type TimeConversionOp = (String, TimeFormatter, TimeFormatter) => String

  def apply(field: String, expr: GroundedFieldExpr, from: String, to: String): GroundedFieldExpr = {
    val (op: TimeConversionOp, input: TimeFormatter, output: TimeFormatter) = (expr.field, from, to) match {
      case (_, "millis", "period") => (millisToPeriod, DefaultDateTimeFormat, DefaultDateTimeFormat)
      case (_, "millis", TimeFormatter(f)) => (millisToPeriod, f, f)
      case (_, null, TimeFormatter(f)) => (millisToPeriod, f, f)
      case (TimeFormatter(inputFormat), null, "millis") => (timeToMillis, inputFormat, inputFormat)
      case (_, "time", TimeFormatter(outputFormat)) => (timeToPeriod, DefaultTimeFormat, outputFormat)
      case _ => throw new IAmSorryDave(s"Unknown conversion $field")
    }
    expr match {
      case e: AggregateFieldExpr => ConvertAggregateExpr(field, e, op, input, output)
      case e: DirectLogFieldExpr => ConvertDirectExpr(field, e, op, input, output)
    }
  }

  val millisToPeriod = (v: String, input: TimeFormatter, output: TimeFormatter) => output.print(new Period(v.toLong))
  val timeToMillis   = (v: String, input: TimeFormatter, output: TimeFormatter) => input.parseDateTime(v).withZoneRetainFields(DateTimeZone.UTC).getMillis.toString
  val timeToPeriod   = (v: String, input: TimeFormatter, output: TimeFormatter) => output.print(input.parseDateTime(v))
}

case class ConvertAggregateExpr(field: String, expr: AggregateFieldExpr, op: ConvertExpr.TimeConversionOp, input: TimeFormatter, output: TimeFormatter)
  extends ConvertOperation with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateFieldExpr] = Seq(expr)
}

case class ConvertDirectExpr(field: String, expr: DirectLogFieldExpr, op: ConvertExpr.TimeConversionOp, input: TimeFormatter, output: TimeFormatter)
  extends ConvertOperation with DirectCalculationExpr {
}