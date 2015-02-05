package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.{SourceRef, IAmSorryDave, LogEntry}

class AutoDetectLog(val types: Seq[LogType[LogEntry]] = Seq()) extends LogType[LogEntry] {

  type P = UnParsedEntry
  
  override def readLinesForNextRecord(lines: LineIterator): UnParsedEntry = {
    if (lines.hasNext) {
      val logType = findType(lines)
      val record = logType.readLinesForNextRecord(lines)
      UnParsedEntry(record, logType)
    } else {
      null
    }
  }

  override def parse(s: UnParsedEntry): LogEntry = s.logType.parse(s.entry.asInstanceOf[s.logType.P])

  private def findType(lines: LineIterator): LogType[LogEntry] = {
    val first = lines.head
    types.collectFirst { case t if t.canParse(first) => t }.getOrElse(throw new IAmSorryDave(s"Can't parse ${lines.headRef} '$first'"))
  }

}

case class UnParsedEntry(entry: LogType[LogEntry]#P, logType: LogType[LogEntry]) extends Sourced {
  override def source: SourceRef = entry.source
}
