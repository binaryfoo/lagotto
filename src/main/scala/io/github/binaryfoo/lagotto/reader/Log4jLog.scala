package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.{Log4jEntry, SourceRef}

/**
 * Log4j records. Assumes they start with [ and can span multiple lines.
 */
object Log4jLog extends LogType[Log4jEntry] {

  override def apply(lines: SourceLineIterator): Log4jEntry = {
    val buffer = new StringBuffer()
    for (line <- lines) {
      if (buffer.length() > 0 && line.startsWith("[")) {
        lines.pushBack(line)
        return Log4jEntry.fromString(buffer.toString, SourceRef(lines.sourceName, lines.lineNumber))
      } else {
        if (buffer.length() > 0)
          buffer.append('\n')
        buffer.append(line)
      }
    }
    if (buffer.length() > 0)
      Log4jEntry.fromString(buffer.toString, SourceRef(lines.sourceName, lines.lineNumber))
    else
      null
  }

}
