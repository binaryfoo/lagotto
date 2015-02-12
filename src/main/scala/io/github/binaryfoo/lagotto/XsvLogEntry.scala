package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

case class XsvLogEntry(simple: SimpleLogEntry, delimiter: Char) extends LogEntry {

  def apply(id: String) = simple.apply(id)

  override def timestamp: DateTime = simple.timestamp

  override def exportAsSeq: Seq[(String, String)] = simple.exportAsSeq

  override def lines: String = simple.lines

}
