package io.github.binaryfoo.lagotto

import org.joda.time.Period

object LogFieldExpr {
  val SubtractOp = """calc\((.+)-(.+)\)""".r

  def unapply(expr: String): Option[GroundedFieldExpr] = {
    Some(expr match {
      case SubtractOp(left, right) => SubtractTimeExpr(left, right)
      case s => DirectLogFieldExpr(s)
    })
  }
}

trait GroundedFieldExpr extends LogFieldExpr {
  def fields: Seq[String]
}

case class DirectLogFieldExpr(field: String) extends GroundedFieldExpr {
  def apply(e: LogLike): String = e(field)
  override def toString(): String = field
  override def fields: Seq[String] = Seq(field)
}

case class SubtractTimeExpr(left: String, right: String) extends GroundedFieldExpr {

  def apply(e: LogLike): String = {
    val format = extractTimeFormat(left)
    val leftTime = format.parseDateTime(e(left))
    val rightTime = format.parseDateTime(e(right))
    val period = new Period(rightTime, leftTime)
    format.print(period)
  }

  private val Aggregated = """^[^(]+\((.+)\)$""".r

  private def extractTimeFormat(field: String): TimeFormatter = field match {
    case Aggregated(TimeFormatter(format)) => format
    case TimeFormatter(format) => format
  }

  override def fields: Seq[String] = Seq(left, right)

  override def toString(): String = s"calc($left-$right)"
}