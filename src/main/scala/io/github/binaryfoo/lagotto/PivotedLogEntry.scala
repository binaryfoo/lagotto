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
class PivotedIterator(val rotateOn: DirectExpr, val pivot: PivotExpr, val aggregates: Seq[FieldExpr], val entries: Iterator[LogEntry]) extends AbstractIterator[PivotedLogEntry] {

  val fields: Seq[String] = Seq(rotateOn.field) ++ pivot.distinctValues().flatMap(v => aggregates.map(v + " - " + _.field))
  private var currentKey: String = null
  private var current: Map[String, Seq[String]] = Map.empty

  def readNext(): PivotedLogEntry = {
    for (e <- entries) {
      val thisKey = rotateOn(e)
      val row = if (thisKey != currentKey) outputRow() else null
      currentKey = thisKey
      current = current.updated(pivot(e), aggregates.map(_(e)))
      if (row != null)
        return row
    }
    outputRow()
  }

  def outputRow() = {
    if (currentKey != null) {
      val row = fields.zip(Seq(currentKey) ++ pivot.distinctValues().flatMap { v =>
        current.getOrElse (v, aggregates.map (_ => "0") )
      }).toMap
      current = Map.empty
      new PivotedLogEntry (row)
    } else {
      null
    }
  }

  override def hasNext: Boolean = entries.hasNext || current.nonEmpty

  override def next(): PivotedLogEntry = readNext()
}
