package io.github.binaryfoo.lagotto

import java.io.{FileWriter, PrintWriter}
import java.util
import java.util.concurrent.atomic.AtomicInteger

import org.joda.time.DateTime

import scala.collection.mutable

/**
 * Like a row in the output of a SQL query with a GROUP BY clause. Has a group key and a set of aggregate values
 * computed over that group.
 *
 * @param key The set of (key, value) pairs that uniquely identify the group.
 * @param aggregates The set of values computed over the group.
 */
case class AggregateLogEntry(key: Map[String, String], aggregates: Seq[(String, String)]) extends LogEntry {

  override def timestamp: DateTime = null
  override def source: SourceRef = null
  override def lines: String = ""

  override def apply(id: String): String = key.getOrElse(id, {
    aggregates.collectFirst { case (k, v) if k == id => v}.orNull
  })

  override def exportAsSeq: Seq[(String, String)] = key.toSeq ++ aggregates
}

trait AggregateOp extends mutable.Builder[LogEntry, String] {
  override def clear() = throw new AbstractionFail

  /**
   * Create a new empty builder of the same type. Don't copy the current builder state.
   */
  def copy(): AggregateOp
}

object AggregateOps {

  val OverExpression = """^[^(]+\((.+)\)$""".r
  val MinOp = """min\((.*)\)""".r
  val MaxOp = """max\((.*)\)""".r
  val SumOp = """sum\((.*)\)""".r
  val AvgOp = """avg\((.*)\)""".r
  val PercentileOp = """percentile\((.*),(.*)\)""".r
  val CountDistinct = """count\(distinct\((.*)\)\)""".r
  val CountIf = """count\((.*)\)""".r
  val GroupConcat = """group_concat\((.*)\)""".r
  val GroupConcatDistinct = """group_concat\(distinct\((.*)\)\)""".r
  val GroupSample = """group_sample\((.*) (\d+)\)""".r
  val GroupIndex = """group_index\((.*) (-?\d+)\)""".r
  val GroupTrace = """group_trace\((.+)\)""".r

  // these need to be vals for equality to work between two AggregateOp instances
  val minLong = (l: Long, r: Long) => math.min(l, r)
  val maxLong = (l: Long, r: Long) => math.max(l, r)
  val minString = (l: String, r: String) => if (l <= r) l else r
  val maxString = (l: String, r: String) => if (l >= r) l else r
  val addNumbers = (l: Double, r: Double) => l + r
}

trait FieldBasedAggregateOp extends AggregateOp {

  def expr: FieldExpr
  def field: String = expr.field
  def add(v: String)

  final override def +=(elem: LogEntry) = {
    val v = expr(elem)
    if (v != null) {
      add(v)
    }
    this
  }
  def copy(): FieldBasedAggregateOp
}

case class CountBuilder() extends AggregateOp {
  
  private var count = 0

  override def +=(elem: LogEntry) = {
    count += 1
    this
  }
  
  override def result(): String = count.toString

  override def toString: String = s"count{count=$count}"

  override def copy() = new CountBuilder
}

case class CountIfBuilder(condition: LogFilter) extends AggregateOp {
  
  private var count = 0
  
  override def +=(elem: LogEntry) = {
    if (condition(elem)) {
      count += 1
    }
    this
  }
  
  override def result(): String = count.toString

  override def toString: String = s"count(if($condition)){count=$count}"

  override def copy() = CountIfBuilder(condition)
}

case class CountDistinctBuilder(expr: DirectExpr) extends FieldBasedAggregateOp {
  
  private val distinctValues = mutable.HashSet[String]()

  override def add(v: String): Unit = distinctValues.add(v)

  override def result(): String = distinctValues.size.toString

  override def toString: String = s"count(distinct($field)){count=${distinctValues.size}}"

  override def copy() = CountDistinctBuilder(expr)
}

case class GroupConcatBuilder(expr: DirectExpr) extends FieldBasedAggregateOp {
  
  private val values = mutable.ListBuffer[String]()

  override def add(v: String): Unit = values += v

  override def result(): String = values.mkString(",")

  override def toString: String = s"group_concat($field){values=$values}"

  override def copy() = GroupConcatBuilder(expr)
}

case class GroupConcatDistinctBuilder(expr: DirectExpr) extends FieldBasedAggregateOp {

  private val values = mutable.HashSet[String]()

  override def add(v: String): Unit = values += v

  override def result(): String = values.toSeq.sorted.mkString(",")

  override def toString: String = s"group_concat(distinct($field)){values=$values}"

  override def copy() = GroupConcatDistinctBuilder(expr)
}

case class GroupSampleBuilder(expr: DirectExpr, size: Int) extends FieldBasedAggregateOp {

  private val values = mutable.ListBuffer[String]()

  override def add(v: String): Unit = values += v

  override def result(): String = {
    val target = Math.min(size, values.size)
    RandomSampler.pickNFromM(target, values.size).map(values(_)).mkString(",")
  }

  override def toString: String = s"group_sample($field,$size){values=$values}"

  override def copy() = GroupSampleBuilder(expr, size)
}

case class GroupIndexBuilder(expr: DirectExpr, index: Int) extends FieldBasedAggregateOp {

  private val values = mutable.ListBuffer[String]()

  override def add(v: String): Unit = values += v

  override def result(): String = {
    val i = if (index < 0) {
      values.size + index
    } else {
      index
    }
    if (i < 0 || i > values.size -1)
      ""
    else {
      values(i)
    }
  }

  override def toString: String = s"group_index($field,$index)"

  override def copy() = GroupIndexBuilder(expr, index)
}

case class GroupTraceBuilder(filePrefix: String, sequence: AtomicInteger = new AtomicInteger()) extends AggregateOp {

  private var fileName: String = _
  private var out: PrintWriter = _

  override def copy(): AggregateOp = {
    GroupTraceBuilder(filePrefix, sequence)
  }

  override def result(): String = {
    if (fileName != null) {
      out.close()
      fileName
    } else {
      ""
    }
  }

  override def +=(elem: LogEntry) = {
    if (fileName == null) {
      open()
    }
    out.println(elem.lines)
    this
  }

  private def open() = {
    fileName = filePrefix + "." + sequence.incrementAndGet() + ".log"
    out = new PrintWriter(new FileWriter(fileName))
  }
}

case class DoubleOpBuilder(expr: FieldExpr, op: (Double, Double) => Double) extends FieldBasedAggregateOp {

  private var current: Option[Double] = None

  override def add(v: String): Unit = {
    current = current match {
      case Some(c) => Some(op(v.toDouble, c))
      case None => Some(v.toDouble)
    }
  }

  override def result(): String = current.map(_.formatted("%f").replace(".000000", "")).getOrElse("")

  override def toString: String = s"doubleOp($field){current=$current}"

  override def copy() = DoubleOpBuilder(expr, op)
}

case class TryLongOpBuilder(expr: DirectExpr, op: (Long, Long) => Long, fallbackOp: (String, String) => String) extends FieldBasedAggregateOp {

  private var useStringOp = false
  private var current: Option[Long] = None
  private var currentFallback: Option[String] = None

  override def add(v: String): Unit = {
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

  override def copy() = TryLongOpBuilder(expr, op, fallbackOp)
}

case class AverageBuilder(expr: DirectExpr) extends FieldBasedAggregateOp {

  private var sum = 0d
  private var count = 0

  override def add(v: String): Unit = {
    sum += v.toDouble
    count += 1
  }

  override def result(): String = if (count == 0) "" else (sum / count).toLong.toString

  override def toString: String = s"avg($field){sum=$sum,count=$count}"

  override def copy() = AverageBuilder(expr)
}

case class PercentileBuilder(percentile: Int, expr: DirectExpr) extends FieldBasedAggregateOp {

  private var values = mutable.ArrayBuffer[Double]()

  override def add(v: String): Unit = {
    values += v.toDouble
  }

  // follows (roughly) https://commons.apache.org/proper/commons-math/javadocs/api-3.0/org/apache/commons/math3/stat/descriptive/rank/Percentile.html
  override def result(): String = {
    val sorted = values.sorted
    val pos = (percentile  * (sorted.size + 1) / 100.0) - 1
    val offset = Math.floor(pos).toInt
    val r = if (offset < 1) {
      sorted(0)
    } else if (offset >= sorted.size - 1) {
      sorted(sorted.size - 1)
    } else {
      val d = pos - offset
      val lower = sorted(offset)
      val upper = sorted(offset + 1)
      lower + (d * (upper - lower))
    }
    r.toString
  }

  override def toString: String = s"percentile($percentile,$field){values=${values.mkString(",")}"

  override def copy() = PercentileBuilder(percentile, expr)
}

/**
 * Accumulate a set of aggregated values for the group uniquely identified by key.
 */
class AggregateLogEntryBuilder(key: Map[String, String], values: Seq[(String, AggregateOp)]) extends mutable.Builder[LogEntry, AggregateLogEntry] {
  
  override def +=(elem: LogEntry) = {
    values.foreach { case (_, v) => v += elem }
    this
  }

  override def result(): AggregateLogEntry = AggregateLogEntry(key, values.map { case (k, v) => (k, v.result()) })

  override def clear(): Unit = throw new AbstractionFail
}

class AbstractionFail extends Exception("Not needed to date. Found a need?")

object RandomSampler {

  def pickNFromM(n: Int, m: Int): Iterable[Int] = {
    assert(n <= m, s"Can't pick $n integers from the range [0-$m)")
    val random = new util.Random()
    val indices = new mutable.HashSet[Int]()

    while (indices.size < n)
      indices.add(random.nextInt(m))

    indices
  }
}
