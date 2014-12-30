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

  /**
   * Create a new empty builder of the same type. Don't copy the current builder state.
   */
  def copy(): AggregateOp
}

object AggregateOp {

  val OverExpression = """^[^(]+\((.+)\)$""".r
  val MinOp = """min\((.*)\)""".r
  val MaxOp = """max\((.*)\)""".r
  val SumOp = """sum\((.*)\)""".r
  val AvgOp = """avg\((.*)\)""".r
  val CountDistinct = """count\(distinct\((.*)\)\)""".r
  val CountIf = """count\((.*)\)""".r
  val GroupConcat = """group_concat\((.*)\)""".r

  /**
   * Unapply or die.
   */
  def operationFor(expr: String): AggregateOp = unapply(expr).get

  def unapply(expr: String): Option[AggregateOp] = {
    val op: AggregateOp = expr match {
      case "count" => new CountBuilder
      case CountDistinct(field) => new CountDistinctBuilder(field)
      case CountIf(LogFilter(condition)) => new CountIfBuilder(condition)
      case MinOp(field) => new TryLongOpBuilder(field, minLong, minString)
      case MaxOp(field) => new TryLongOpBuilder(field, maxLong, maxString)
      case SumOp(field) => new LongOpBuilder(field, addLongs)
      case AvgOp(field) => new AverageBuilder(field)
      case GroupConcat(field) => new GroupConcatBuilder(field)
      case _ => null
    }
    Option(op)
  }

  // these need to be vals for equality to work between two AggregateOp instances
  val minLong = (l: Long, r: Long) => math.min(l, r)
  val maxLong = (l: Long, r: Long) => math.max(l, r)
  val minString = (l: String, r: String) => if (l <= r) l else r
  val maxString = (l: String, r: String) => if (l >= r) l else r
  val addLongs = (l: Long, r: Long) => l + r
}

object AggregateLogLike {

  /**
   * Apply aggregation 'decorator' if something in outputFields requires it.
   * @param s The stream of log entries that will be output.
   * @param keyFields The set of fields identifying each group.
   * @param aggregateFields The set of aggregates to calculate for each group.
   * @return The original stream s or a Stream[AggregateLogLike].
   */
  def aggregate(s: Iterator[LogLike], keyFields: Seq[GroundedFieldExpr], aggregateFields: Seq[AggregateFieldExpr]): Stream[LogLike] = {
    def keyFor(e: LogLike): Seq[(String, String)] = {
      for {
        k <- keyFields
      } yield (k.field, k(e))
    }
    def newBuilder(k: Seq[(String, String)]) = {
      // Each aggregate holds state so needs to be cloned for each new group
      val aggregates = aggregateFields.map(e => (e.field, e.op.copy()))
      new AggregateLogLikeBuilder(k.toMap, aggregates)
    }
    OrderedGroupBy.groupByOrdered(s, keyFor, newBuilder).values.toStream
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

case class CountBuilder() extends AggregateOp {
  
  private var count = 0

  override def +=(elem: LogLike) = {
    count += 1
    this
  }
  
  override def result(): String = count.toString

  override def toString: String = s"count{count=$count}"

  override def copy() = new CountBuilder
}

case class CountIfBuilder(condition: FieldFilter) extends AggregateOp {
  
  private var count = 0
  
  override def +=(elem: LogLike) = {
    if (condition(elem)) {
      count += 1
    }
    this
  }
  
  override def result(): String = count.toString

  override def toString: String = s"count(if($condition)){count=$count}"

  override def copy() = new CountIfBuilder(condition)
}

case class CountDistinctBuilder(field: String) extends FieldBasedAggregateOp {
  
  private val distinctValues = mutable.HashSet[String]()

  override def add(v: String) = distinctValues.add(v)

  override def result(): String = distinctValues.size.toString

  override def toString: String = s"count(distinct($field)){count=${distinctValues.size}}"

  override def copy() = new CountDistinctBuilder(field)
}

case class GroupConcatBuilder(field: String) extends FieldBasedAggregateOp {
  
  private val values = mutable.ListBuffer[String]()

  override def add(v: String) = values += v

  override def result(): String = values.mkString(",")

  override def toString: String = s"group_concat($field){values=$values}"

  override def copy() = new GroupConcatBuilder(field)
}

case class LongOpBuilder(field: String, op: (Long, Long) => Long) extends FieldBasedAggregateOp {

  private var current: Option[Long] = None

  override def add(v: String) = {
    current = current match {
      case Some(c) => Some(op(v.toLong, c))
      case None => Some(v.toLong)
    }
  }

  override def result(): String = current.getOrElse("").toString

  override def toString: String = s"longOp($field){current=$current}"

  override def copy() = new LongOpBuilder(field, op)
}

case class TryLongOpBuilder(field: String, op: (Long, Long) => Long, fallbackOp: (String, String) => String) extends FieldBasedAggregateOp {

  private var useStringOp = false
  private var current: Option[Long] = None
  private var currentFallback: Option[String] = None

  override def add(v: String) = {
    if (useStringOp) {
      addWithStringOp(v)
    } else {
      try {
        addWithLongOp(v.toLong)
      }
      catch {
        case e: NumberFormatException =>
          useStringOp = true
          currentFallback = current.map(_.toString)
          addWithStringOp(v)
      }
    }
    
  }

  def addWithLongOp(v: Long): Unit = {
    current = current match {
      case Some(c) => Some(op(v, c))
      case None => Some(v)
    }
  }

  def addWithStringOp(v: String): Unit = {
    currentFallback = currentFallback match {
      case Some(c) => Some(fallbackOp(v, c))
      case None => Some(v)
    }
  }

  override def result(): String = current.map(_.toString).getOrElse(currentFallback.getOrElse(""))

  override def toString: String = s"tryIntegerOp($field){current=$current,currentFallback=$currentFallback}"

  override def copy() = new TryLongOpBuilder(field, op, fallbackOp)
}

case class AverageBuilder(field: String) extends FieldBasedAggregateOp {

  private var sum = 0
  private var count = 0

  override def add(v: String) = {
    sum += v.toInt
    count += 1
  }

  override def result(): String = if (count == 0) "" else (sum / count).toString

  override def toString: String = s"avg($field){sum=$sum,count=$count}"

  override def copy() = new AverageBuilder(field)
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
