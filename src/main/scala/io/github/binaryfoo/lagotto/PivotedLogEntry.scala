package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.AbstractIterator

case class PivotedLogEntry(row: Map[String, String]) extends LogEntry {

  override def timestamp: DateTime = null

  override def exportAsSeq: Seq[(String, String)] = row.toSeq

  override def lines: String = ""

  override def apply(id: String): String = row(id)

}

class PivotedIterator(val rotateOn: DirectExpr, val pivot: PivotExpr, val aggregates: Seq[AggregateExpr], val entries: Iterator[LogEntry]) extends AbstractIterator[PivotedLogEntry] {

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
