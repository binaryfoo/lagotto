package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.LogLike

object AutoDetectLog extends LogType[LogLike] {

  override def apply(lines: SourceLineIterator): LogLike = {
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
