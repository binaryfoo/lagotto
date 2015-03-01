package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.AbstractIterator

case class PivotedLogEntry(row: Map[String, String]) extends LogEntry {

  override def timestamp: DateTime = null
  override def source: SourceRef = null
  override def exportAsSeq: Seq[(String, String)] = row.toSeq
  override def lines: String = ""
  override def apply(id: String): String = row(id)

}

/**
 * A single value for one of the N rows included in a single PivotedLogEntry.
 */
case class PivotedValue(field: String, value: String) extends LogEntry {
  override def timestamp: DateTime = null
  override def source: SourceRef = null
  override def exportAsSeq: Seq[(String, String)] = null
  override def lines: String = ""
  override def apply(id: String): String = {
    if (field != id) {
      throw new IllegalArgumentException(s"Wrong value being queried $id != $field")
    }
    value
  }
}

/**
 * Supports a use case like:
 *
 *   time(HH:mm),pivot(mti),count
 *
 * Instead of getting each time,mti,count pair in the vertical you get something like:
 *
 *   time(HH:mm),0200 - count,0210 - count,0400 - count, 0410 - count
 *   13:59,10,9,1,1
 *
 * Without the pivot you'd have:
 *
 *   time(HH:mm),mti,count
 *   13:59,0200,10
 *   13:59,0210,9
 *   13:59,0400,1
 *   13:59,0410,1
 */
class PivotedIterator(val rotateOn: DirectExpr, val pivot: PivotExpr, val pivoted: Seq[FieldExpr], val entries: Iterator[LogEntry]) extends AbstractIterator[PivotedLogEntry] {

  private val (aggregateOfPivot, toPivot) = pivoted.partition {
    case a@AggregateExpr(_, _) => a.expr.exists(_.isInstanceOf[PivotResultExpr])
    case _ => false
  }
  private val aggregateOps: Seq[PivotAggregate] = extractAggregateOps
  private val (pivotedFields, pivotedLookup) = pivotResultFields
  val fields: Seq[String] = Seq(rotateOn.field) ++ pivotedFields ++ aggregateOfPivot.map(_.field)
  // value of pivot expr
  private var currentKey: String = null
  // pivot expr -> values of toPivot
  // will be flattened into a single row
  private var current: Map[String, Seq[String]] = Map.empty

  def readNext(): PivotedLogEntry = {
    for (e <- entries) {
      val thisKey = rotateOn(e)
      val row = if (thisKey != currentKey) outputRow() else null
      currentKey = thisKey
      current = current.updated(pivot(e), toPivot.map(_(e)))
      if (row != null)
        return row
    }
    outputRow()
  }

  def outputRow() = {
    if (currentKey != null) {
      val pivotedRow = fields.zip(Seq(currentKey) ++ pivot.distinctValues().flatMap { v =>
        current.getOrElse(v, toPivot.map(_ => "0"))
      }).toMap
      val rowAggregates = aggregateOps.map { case PivotAggregate(resultName, field, op) =>
        val a = op.copy()
        pivotedLookup(field).foreach(name => a += PivotedValue(field, pivotedRow(name)))
        (resultName, a.result())
      }
      current = Map.empty
      new PivotedLogEntry(pivotedRow ++ rowAggregates)
    } else {
      null
    }
  }

  override def hasNext: Boolean = entries.hasNext || current.nonEmpty

  override def next(): PivotedLogEntry = readNext()

  private def extractAggregateOps: Seq[PivotAggregate] = {
    aggregateOfPivot.map {
      case AggregateExpr(resultName, op) =>
        op match {
          case o: FieldBasedAggregateOp =>
            val pivotedField = o.expr.asInstanceOf[PivotResultExpr].pivotedField
            if (!toPivot.exists(_.field == pivotedField))
              throw new IAmSorryDave(s"$pivotedField must be in the field list to calculate ${o.field}")
            PivotAggregate(resultName, pivotedField, op)
          case CountIfBuilder(condition@FieldFilterOn(expr)) =>
            val pivotedField = expr.field
            if (!toPivot.exists(_.field == pivotedField))
              throw new IAmSorryDave(s"$pivotedField must be in the field list to calculate count(if($condition))")
            PivotAggregate(resultName, pivotedField, op)
        }
      case x => throw new IAmSorryDave(s"Can't process $x")
    }
  }

  private def pivotResultFields: (Seq[String], Map[String, Seq[String]]) = {
    val pivoted: Seq[(String, String)] = pivot.distinctValues().flatMap(v => toPivot.map(p => p.field -> (v + " - " + p.field)))
    val pivotResultFields = pivoted.map(_._2)
    val pivotResultLookup = pivoted.groupBy(_._1).mapValues(_.map(_._2))
    (pivotResultFields, pivotResultLookup)
  }

}

case class PivotAggregate(resultName: String, pivotField: String, op: AggregateOp)
