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
  val AvgOp = """avg\((.*)\)""".r
  val CountIf = """count\((.*)\)""".r
  val GroupConcat = """group_concat\((.*)\)""".r

  type AggregateOp = List[LogLike] => String

  private def collectIntegers(values: List[LogLike], field: String) = values.flatMap{e =>
    val v = e(field)
    if (v == null) None
    else Some(v.toInt)
  }

  def operationFor(expr: String): Option[AggregateOp] = {
    val op: AggregateOp = expr match {
        case "count" => _.size.toString
        case CountIf(LogFilter(condition)) => _.count(condition).toString
        case MinOp(field) => collectIntegers(_, field) match {
          case Nil => ""
          case l: List[Int] => l.min.toString
        }
        case MaxOp(field) => collectIntegers(_, field) match {
          case Nil => ""
          case l: List[Int] => l.max.toString
        }
        case SumOp(field) => collectIntegers(_, field).sum.toString
        case AvgOp(field) => values => (collectIntegers(values, field).sum / values.size).toString
        case GroupConcat(field) => _.map(_(field)).filter(_ != null).mkString(",")
        case _ => null
      }
    Option(op)
  }

  /**
   * Apply aggregation 'decorator' if something in outputFields requires it.
   * @param s The stream of log entries that will be output.
   * @param outputFields The set of fields that will be output.
   * @return The original stream s or a Stream[AggregateLogLike].
   */
  def aggregate(s: Stream[LogLike], outputFields: Seq[String]): Stream[LogLike] = {
    val aggregateFields = outputFields.filter(operationFor(_).isDefined).toSet
    if (aggregateFields.isEmpty) {
      s
    } else {
      val keyFields = outputFields.filterNot(aggregateFields)
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
