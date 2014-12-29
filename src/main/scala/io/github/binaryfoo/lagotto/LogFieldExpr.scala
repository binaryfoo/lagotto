package io.github.binaryfoo.lagotto

import org.joda.time.Period

object LogFieldExpr {
  val SubtractOp = """calc\((.+)-(.+)\)""".r
  val DivideOp = """calc\((.+)/(.+)\)""".r

  def unapply(expr: String): Option[GroundedFieldExpr] = {
    Some(expr match {
      case field@SubtractOp(LogFieldExpr(left), LogFieldExpr(right)) => SubtractTimeExpr(field, left, right)
      case field@DivideOp(LogFieldExpr(left), LogFieldExpr(right)) => DivideExpr(field, left, right)
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

case class SubtractTimeExpr(field: String, left: GroundedFieldExpr, right: GroundedFieldExpr) extends GroundedFieldExpr with CanRequireAggregates {

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

  override def children(): Seq[GroundedFieldExpr] = Seq(left, right)
}

case class DivideExpr(field: String, left: GroundedFieldExpr, right: GroundedFieldExpr) extends GroundedFieldExpr with CanRequireAggregates {
  def apply(e: LogLike): String = {
    val leftNumber = left(e).toDouble
    val rightNumber = right(e).toDouble
    (leftNumber / rightNumber).formatted("%.4f")
  }
  override def children(): Seq[GroundedFieldExpr] = Seq(left, right)
}