package io.github.binaryfoo.lagotto

import org.joda.time.{DateTimeZone, Period}

object LogFieldExpr {
  val SubtractOp = """calc\((.+)-(.+)\)""".r
  val DivideOp = """calc\((.+)/(.+)\)""".r
  val ConvertOp = """\(([^ ]+) (?:(.+) )?as (.+)\)""".r

  def unapply(expr: String): Option[GroundedFieldExpr] = {
    Some(expr match {
      case field@SubtractOp(LogFieldExpr(left@TimeFormatter(leftFormat)), LogFieldExpr(right@TimeFormatter(rightFormat))) => SubtractTwoTimesExpr(field, left, right, leftFormat, rightFormat)
      case field@SubtractOp(LogFieldExpr(left@TimeFormatter(leftFormat)), LogFieldExpr(right)) => SubtractMillisFromTimeExpr(field, left, right, leftFormat)
      case field@DivideOp(LogFieldExpr(left), LogFieldExpr(right)) => DivideExpr(field, left, right)
      case field@ConvertOp(LogFieldExpr(child), from, to) => ConvertExpr(field, child, from, to)
      case "delay" => DelayFieldExpr
      case field@AggregateOp(op) => AggregateFieldExpr(field, op)
      case s => DirectLogFieldExpr(s)
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

/**
 * Exists separately from LogFieldExpr to allow passing lambas to LogLike.toXsv(). Maybe misguided.
 */
trait GroundedFieldExpr extends LogFieldExpr {
  def field: String
  override def toString(): String = field
}

case class DirectLogFieldExpr(field: String) extends GroundedFieldExpr {
  def apply(e: LogLike): String = e(field)
}

object DelayFieldExpr extends GroundedFieldExpr {
  val field = "delay"
  def apply(e: LogLike): String = {
    if (!e.isInstanceOf[DelayTimer]) {
      throw new IllegalStateException(s"We lost delay calculation. Can't retrieve delay from $e")
    }
    e(field)
  }
}

case class AggregateFieldExpr(field: String, op: AggregateOp) extends GroundedFieldExpr {
  override def apply(e: LogLike): String = {
    if (!e.isInstanceOf[AggregateLogLike]) {
      throw new IllegalStateException(s"We lost aggregation. Can't retrieve $field from $e")
    }
    e(field)
  }
}

trait CanRequireAggregates {
  def aggregates(): Seq[AggregateFieldExpr] = {
    children().collect {
      case e: AggregateFieldExpr => e
    }
  }
  def children(): Seq[GroundedFieldExpr]
}

object HasAggregateExpressions {
  def unapply(expr: GroundedFieldExpr): Option[Seq[AggregateFieldExpr]] = {
    expr match {
      case e: CanRequireAggregates =>
        val children = e.aggregates()
        if (children.isEmpty) None else Some(children)
      case e: AggregateFieldExpr => Some(Seq(e))
      case _ => None
    }
  }
}

/**
 * Handles the fact that the aggregation process does the calculation.
 * Trying to perform the calculation on the output of aggregation would fail because the underlying fields are gone.
 * Unless the calculation is happening over aggregated fields.
 */
trait CalculatedFieldExpr extends GroundedFieldExpr with CanRequireAggregates {
  final def apply(e: LogLike): String = {
    // TODO: currently fails if one field is an aggregate but the other not... Best solved statically in LogFieldExpr.unapply() ?
    e match {
      case aggregated: AggregateLogLike if aggregates().isEmpty => e(field)
      case _ => calculate(e)
    }
  }
  def calculate(e: LogLike): String
}

/**
 * Show the difference between two timestamps as a period.
 */
case class SubtractTwoTimesExpr(field: String, left: GroundedFieldExpr, right: GroundedFieldExpr, leftFormat: TimeFormatter, rightFormat: TimeFormatter) extends CalculatedFieldExpr {

  def calculate(e: LogLike): String = {
    val leftTime = leftFormat.parseDateTime(left(e))
    val rightTime = rightFormat.parseDateTime(right(e))
    val period = new Period(rightTime, leftTime)
    leftFormat.print(period)
  }

  override def children(): Seq[GroundedFieldExpr] = Seq(left, right)
}

/**
 * Show a new timestamp that a number of milliseconds (right) prior to the original (left).
 */
case class SubtractMillisFromTimeExpr(field: String, left: GroundedFieldExpr, right: GroundedFieldExpr, leftFormat: TimeFormatter) extends CalculatedFieldExpr {

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

  override def children(): Seq[GroundedFieldExpr] = Seq(left, right)
}

case class DivideExpr(field: String, left: GroundedFieldExpr, right: GroundedFieldExpr) extends CalculatedFieldExpr {
  def calculate(e: LogLike): String = {
    val leftNumber = left(e).toDouble
    val rightNumber = right(e).toDouble
    (leftNumber / rightNumber).formatted("%.4f")
  }
  override def children(): Seq[GroundedFieldExpr] = Seq(left, right)
}

/**
 * A limited set of type conversions.
 */
case class ConvertExpr(field: String, expr: GroundedFieldExpr, from: String, to: String) extends CalculatedFieldExpr {
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

  override def children(): Seq[GroundedFieldExpr] = Seq(expr)
}