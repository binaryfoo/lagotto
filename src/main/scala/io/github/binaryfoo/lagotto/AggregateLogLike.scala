package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.AggregateLogLike.AggregateOp
import org.joda.time.DateTime

case class AggregateLogLike(key: Map[String, String], values: List[LogLike]) extends LogLike {

  override def timestamp: DateTime = values.head.timestamp

  override def lines: String =  values.map(_.lines).mkString("<group>\n", "\n", "\n</group>")

  override def apply(id: String): String = key.getOrElse(id, {
    val op: AggregateOp = AggregateLogLike.operationFor(id).get
    op(values)
  })

}

object AggregateLogLike {

  val MinOp = """min\((.*)\)""".r
  val MaxOp = """max\((.*)\)""".r
  val SumOp = """sum\((.*)\)""".r
  val CountIf = """count\((.*)\)""".r
  val GroupConcat = """group_concat\((.*)\)""".r

  type AggregateOp = List[LogLike] => String

  private def collectNonNull(values: List[LogLike], field: String) = values.flatMap(e => Option(e(field)))

  def operationFor(expr: String): Option[AggregateOp] = {
    val op: AggregateOp = expr match {
        case "count" => _.size.toString
        case CountIf(LogFilter(condition)) => _.count(condition).toString
        case MinOp(field) => collectNonNull(_, field) match {
          case Nil => ""
          case l: List[String] => l.map(_.toInt).min.toString
        }
        case MaxOp(field) => collectNonNull(_, field) match {
          case Nil => ""
          case l: List[String] => l.map(_.toInt).max.toString
        }
        case SumOp(field) => collectNonNull(_, field).map(_.toInt).sum.toString
        case GroupConcat(field) => _.map(_(field)).filter(_ != null).mkString(",")
        case _ => null
      }
    Option(op)
  }

  def aggregate(s: Stream[LogLike], outputFields: Seq[String]): Stream[LogLike] = {
    val keyFields = outputFields.filter(operationFor(_).isEmpty)
    if (keyFields.isEmpty) {
      s
    } else {
      def keyFor(e: LogLike): Seq[(String, String)] = {
        for {
          k <- keyFields
        } yield (k, e(k))
      }
      OrderedGroupBy.groupByOrdered(s, keyFor).map {
        case (key, values) => AggregateLogLike(key.toMap, values)
      }.toStream
    }
  }

}
