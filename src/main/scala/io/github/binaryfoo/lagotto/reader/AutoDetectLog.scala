package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.{SourceRef, IAmSorryDave, LogEntry}

class AutoDetectLog(val types: Seq[LogType[LogEntry]] = Seq()) extends LogType[LogEntry] {

  override def readLinesForNextRecord(lines: SourceLineIterator): LineSet = {
    if (lines.hasNext) {
      findType(lines.peek(), lines.sourceRef).readLinesForNextRecord(lines)
    } else {
      null
    }
  }

  override def parse(s: LineSet): LogEntry = findType(s.lines.head, s.source).parse(s)

  def findType(first: String, ref: SourceRef): LogType[LogEntry] = {
    val logType = types.collectFirst {
      case t if t.canParse(first) => t
    }
      .getOrElse(throw new IAmSorryDave(s"Can't parse $ref '$first'"))
    logType
  }

}
