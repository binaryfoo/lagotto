package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.mutable

/**
 * Like a row in the output of a SQL query with a GROUP BY clause. Has a group key and a set of aggregate values
 * computed over that group.
 *
 * @param key The set of (key, value) pairs that uniquely identify the group.
 * @param aggregates The set of values computed over the group.
 */
case class AggregateLogLike(key: Map[String, String], aggregates: Seq[(String, String)]) extends LogLike {

  override def timestamp: DateTime = ???

  override def lines: String =  ???

  override def apply(id: String): String = key.getOrElse(id, {
    aggregates.collectFirst { case (k, v) if k == id => v}.orNull
  })

}

trait AggregateOp extends mutable.Builder[LogLike, String] {
  override def clear() = ???
}

object AggregateLogLike {

  val MinOp = """min\((.*)\)""".r
  val MaxOp = """max\((.*)\)""".r
  val SumOp = """sum\((.*)\)""".r
  val AvgOp = """avg\((.*)\)""".r
  val CountDistinct = """count\(distinct\((.*)\)\)""".r
  val CountIf = """count\((.*)\)""".r
  val GroupConcat = """group_concat\((.*)\)""".r

  def operationFor(expr: String): Option[AggregateOp] = {
    val op: AggregateOp = expr match {
        case "count" => new CountBuilder
        case CountDistinct(field) => new CountDistinctBuilder(field)
        case CountIf(LogFilter(condition)) => new CountIfBuilder(condition)
        case MinOp(field) => new IntegerOpBuilder(field, math.min)
        case MaxOp(field) => new IntegerOpBuilder(field, math.max)
        case SumOp(field) => new IntegerOpBuilder(field, _ + _)
        case AvgOp(field) => new AverageBuilder(field)
        case GroupConcat(field) => new GroupConcatBuilder(field)
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
  def aggregate(s: Iterator[LogLike], outputFields: Seq[String]): Stream[LogLike] = {
    val aggregateFields = outputFields.filter(operationFor(_).isDefined).toSet
    if (aggregateFields.isEmpty) {
      s.toStream
    } else {
      val keyFields = outputFields.filterNot(aggregateFields)
      def keyFor(e: LogLike): Seq[(String, String)] = {
        for {
          k <- keyFields
        } yield (k, e(k))
      }
      def newBuilder(k: Seq[(String, String)]) = {
        val aggregates = outputFields.flatMap(field => operationFor(field).map(op => (field, op)))
        new AggregateLogLikeBuilder(k.toMap, aggregates)
      }
      OrderedGroupBy.groupByOrdered(s, keyFor, newBuilder).values.toStream
    }
  }

}

trait FieldBasedAggregateOp extends AggregateOp {

  def field: String
  def add(v: String)

  final override def +=(elem: LogLike) = {
    val v = elem(field)
    if (v != null) {
      add(v)
    }
    this
  }

}

class CountBuilder extends AggregateOp {
  
  private var count = 0

  override def +=(elem: LogLike) = {
    count += 1
    this
  }
  
  override def result(): String = count.toString
}

class CountIfBuilder(val condition: FieldFilter) extends AggregateOp {
  
  private var count = 0
  
  override def +=(elem: LogLike) = {
    if (condition(elem)) {
      count += 1
    }
    this
  }
  
  override def result(): String = count.toString
}

class CountDistinctBuilder(val field: String) extends FieldBasedAggregateOp {
  
  private val distinctValues = mutable.HashSet[String]()

  override def add(v: String) = distinctValues.add(v)

  override def result(): String = distinctValues.size.toString

}

class GroupConcatBuilder(val field: String) extends FieldBasedAggregateOp {
  
  private val values = mutable.ListBuffer[String]()

  override def add(v: String) = values += v

  override def result(): String = values.mkString(",")

}

class IntegerOpBuilder(val field: String, val op: (Int, Int) => Int) extends FieldBasedAggregateOp {

  private var current: Option[Int] = None

  override def add(v: String) = {
    current = current match {
      case Some(c) => Some(op(v.toInt, c))
      case None => Some(v.toInt)
    }
  }

  override def result(): String = current.getOrElse("").toString
}

class AverageBuilder(val field: String) extends FieldBasedAggregateOp {

  private var sum = 0
  private var count = 0

  override def add(v: String) = {
    sum += v.toInt
    count += 1
  }

  override def result(): String = if (count == 0) "" else (sum / count).toString
}

/**
 * Accumulate a set of aggregated values for the group uniquely identified by key.
 */
class AggregateLogLikeBuilder(key: Map[String, String], values: Seq[(String, AggregateOp)]) extends mutable.Builder[LogLike, AggregateLogLike] {
  
  override def +=(elem: LogLike) = {
    values.foreach { case (_, v) => v += elem }
    this
  }

  override def result(): AggregateLogLike = AggregateLogLike(key, values.map { case (k, v) => (k, v.result()) })

  override def clear(): Unit = ???
}
