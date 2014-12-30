package io.github.binaryfoo.lagotto

import org.joda.time.{DateTimeZone, Period}

object LogFieldExpr {
  val SubtractOp = """calc\((.+)-(.+)\)""".r
  val DivideOp = """calc\((.+)/(.+)\)""".r
  val ConvertOp = """\(([^ ]+) (?:(.+) )?as (.+)\)""".r

  def unapply(expr: String): Option[GroundedFieldExpr] = {
    Some(expr match {
      case SubtractOp(AggregateFieldExpr(left), AggregateFieldExpr(right)) =>
        val leftFormat = left.field match {
          case AggregateOp.OverExpression(TimeFormatter(format)) => format
          case _ => throw new IAmSorryDave(s"In calc(left-right) left must be time expression. ${left.field} is not a time expression.")
        }
        right.field match {
          case AggregateOp.OverExpression(TimeFormatter(rightFormat)) => SubtractTwoAggregateTimesExpr(expr, left, right, leftFormat, rightFormat)
          case _ => SubtractAggregateMillisFromTimeExpr(expr, left, right, leftFormat)
        }
      case SubtractOp(DirectLogFieldExpr(left), DirectLogFieldExpr(right)) =>
        val leftFormat = left.field match {
          case TimeFormatter(format) => format
          case _ => throw new IAmSorryDave(s"In calc(left-right) left must be time expression. ${left.field} is not a time expression.")
        }
        right.field match {
          case TimeFormatter(rightFormat) => SubtractTwoDirectTimesExpr(expr, left, right, leftFormat, rightFormat)
          case _ => SubtractDirectMillisFromTimeExpr(expr, left, right, leftFormat)
        }
      case DivideOp(AggregateFieldExpr(left), AggregateFieldExpr(right)) => DivideAggregatesExpr(expr, left, right)
      case DivideOp(DirectLogFieldExpr(left), DirectLogFieldExpr(right)) => DivideDirectExpr(expr, left, right)
      case ConvertOp(AggregateFieldExpr(child), from, to) => ConvertAggregateExpr(expr, child, from, to)
      case ConvertOp(DirectLogFieldExpr(child), from, to) => ConvertDirectExpr(expr, child, from, to)
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
  def from: String
  def to: String

  def calculate(e: LogLike): String = {
    val value = expr(e)
    if (value == null || value == "")
      null
    else
      (expr.field, from, to) match {
        case (_, "millis", "period") =>
          val period = new Period(value.toLong)
          DefaultDateTimeFormat.print(period)
        case (_, "millis", TimeFormatter(format)) =>
          val period = new Period(value.toLong)
          format.print(period)
        case (_, null, TimeFormatter(format)) =>
          val period = new Period(value.toLong)
          format.print(period)
        case (TimeFormatter(inputFormat), null, "millis") =>
          val utcDateTime = inputFormat.parseDateTime(value).withZoneRetainFields(DateTimeZone.UTC)
          utcDateTime.getMillis.toString
        case (_, "time", TimeFormatter(outputFormat)) =>
          outputFormat.print(DefaultTimeFormat.parseDateTime(value))
    }
  }
}

case class ConvertAggregateExpr(field: String, expr: AggregateFieldExpr, from: String, to: String)
  extends ConvertOperation with CalculationOverAggregates {
  override def dependencies(): Seq[AggregateFieldExpr] = Seq(expr)
}

case class ConvertDirectExpr(field: String, expr: DirectLogFieldExpr, from: String, to: String)
  extends ConvertOperation with DirectCalculationExpr {
}