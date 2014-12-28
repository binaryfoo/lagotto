package io.github.binaryfoo.lagotto

import org.joda.time.Period

object LogFieldExpr {
  val SubtractOp = """calc\((.+)-(.+)\)""".r

  def unapply(expr: String): Option[GroundedFieldExpr] = {
    Some(expr match {
      case field@SubtractOp(LogFieldExpr(left), LogFieldExpr(right)) => SubtractTimeExpr(field, left, right)
      case "delay" => DelayFieldExpr
      case field@AggregateOp(op) => AggregateFieldExpr(field, op)
      case s => DirectLogFieldExpr(s)
    })
  }
}

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

case class SubtractTimeExpr(field: String, left: GroundedFieldExpr, right: GroundedFieldExpr) extends GroundedFieldExpr {

  def apply(e: LogLike): String = {
    val format = extractTimeFormat(left.field)
    val leftTime = format.parseDateTime(left(e))
    val rightTime = format.parseDateTime(right(e))
    val period = new Period(rightTime, leftTime)
    format.print(period)
  }

  private val Aggregated = """^[^(]+\((.+)\)$""".r

  private def extractTimeFormat(field: String): TimeFormatter = field match {
    case Aggregated(TimeFormatter(format)) => format
    case TimeFormatter(format) => format
  }
}