package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.LogEntry

object AutoDetectLog extends LogType[LogEntry] {

  override def apply(lines: SourceLineIterator): LogEntry = {
    if (lines.hasNext) {
      val first = lines.peek()
      if (first.charAt(0) == '[')
        Log4jLog(lines)
      else
        JposLog(lines)
    } else {
      null
    }
  }
}
