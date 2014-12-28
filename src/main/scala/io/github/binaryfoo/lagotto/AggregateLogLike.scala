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

object AggregateOp {

  val MinOp = """min\((.*)\)""".r
  val MaxOp = """max\((.*)\)""".r
  val SumOp = """sum\((.*)\)""".r
  val AvgOp = """avg\((.*)\)""".r
  val CountDistinct = """count\(distinct\((.*)\)\)""".r
  val CountIf = """count\((.*)\)""".r
  val GroupConcat = """group_concat\((.*)\)""".r
  val MinStringOp = """minStr\((.*)\)""".r
  val MaxStringOp = """maxStr\((.*)\)""".r

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
      case MinStringOp(field) => new StringOpBuilder(field, (l, r) => if (l <= r) l else r)
      case MaxStringOp(field) => new StringOpBuilder(field, (l, r) => if (l >= r) l else r)
      case _ => null
    }
    Option(op)
  }

  def unapply(expr: String): Option[AggregateOp] = operationFor(expr)
}

object AggregateLogLike {

  /**
   * Apply aggregation 'decorator' if something in outputFields requires it.
   * @param s The stream of log entries that will be output.
   * @param outputFields The set of fields that will be output.
   * @return The original stream s or a Stream[AggregateLogLike].
   */
  def aggregate(s: Iterator[LogLike], outputFields: Seq[GroundedFieldExpr]): Stream[LogLike] = {
    val (aggregateFields, keyFields) = outputFields.partition {
      case AggregateFieldExpr(_, _) => true
      case SubtractTimeExpr(_, AggregateFieldExpr(_, _), AggregateFieldExpr(_, _)) => true
      case _ => false
    }
    if (aggregateFields.isEmpty) {
      s.toStream
    } else {
      def keyFor(e: LogLike): Seq[(String, String)] = {
        for {
          k <- keyFields
        } yield (k.field, k(e))
      }
      def newBuilder(k: Seq[(String, String)]) = {
        val aggregates = aggregateFields.flatMap {
          // TODO remove whackness of calling AggregateOp.operationFor() by making the Builder used by OrderedGroupBy immutable
          case SubtractTimeExpr(_, AggregateFieldExpr(left, _), AggregateFieldExpr(right, _)) =>
            Seq((left, AggregateOp.operationFor(left).get), (right, AggregateOp.operationFor(right).get))
          case AggregateFieldExpr(field, _) =>
            Seq((field, AggregateOp.operationFor(field).get))
        }
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

  override def toString: String = s"count{count=$count}"
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

  override def toString: String = s"count(if($condition)){count=$count}"
}

class CountDistinctBuilder(val field: String) extends FieldBasedAggregateOp {
  
  private val distinctValues = mutable.HashSet[String]()

  override def add(v: String) = distinctValues.add(v)

  override def result(): String = distinctValues.size.toString

  override def toString: String = s"count(distinct($field)){count=${distinctValues.size}}"
}

class GroupConcatBuilder(val field: String) extends FieldBasedAggregateOp {
  
  private val values = mutable.ListBuffer[String]()

  override def add(v: String) = values += v

  override def result(): String = values.mkString(",")

  override def toString: String = s"group_concat($field){values=$values}"
}

class IntegerOpBuilder(val field: String, val op: (Long, Long) => Long) extends FieldBasedAggregateOp {

  private var current: Option[Long] = None

  override def add(v: String) = {
    current = current match {
      case Some(c) => Some(op(v.toLong, c))
      case None => Some(v.toLong)
    }
  }

  override def result(): String = current.getOrElse("").toString

  override def toString: String = s"integerOp($field){current=$current}"
}

class StringOpBuilder(val field: String, val op: (String, String) => String) extends FieldBasedAggregateOp {

  private var current: Option[String] = None

  override def add(v: String) = {
    current = current match {
      case Some(c) => Some(op(v, c))
      case None => Some(v)
    }
  }

  override def result(): String = current.getOrElse("")

  override def toString: String = s"stringOp($field){current=$current}"
}

class AverageBuilder(val field: String) extends FieldBasedAggregateOp {

  private var sum = 0
  private var count = 0

  override def add(v: String) = {
    sum += v.toInt
    count += 1
  }

  override def result(): String = if (count == 0) "" else (sum / count).toString

  override def toString: String = s"avg($field){sum=$sum,count=$count}"
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
