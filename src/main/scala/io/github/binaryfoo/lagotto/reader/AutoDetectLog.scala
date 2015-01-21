package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.{IAmSorryDave, LogEntry}

class AutoDetectLog(val types: Seq[LogType[LogEntry]] = Seq()) extends LogType[LogEntry] {

  override def apply(lines: SourceLineIterator): LogEntry = {
    if (lines.hasNext) {
      val first = lines.peek()
      types.collectFirst {
        case t if t.canParse(first, lines.sourceName) => t
      }
        .map(_(lines))
        .getOrElse(throw new IAmSorryDave(s"Can't parse ${lines.sourceRef} '$first'"))
    } else {
      null
    }
  }
}
