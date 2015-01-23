package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.Log4jEntry

/**
 * Log4j records. Assumes they start with [ and can span multiple lines.
 */
object Log4jLog extends LogType[Log4jEntry] {

  override def readLinesForNextRecord(lines: SourceLineIterator): LineSet = {
    val buffer = new StringBuffer()
    for (line <- lines) {
      if (buffer.length() > 0 && line.startsWith("[")) {
        lines.pushBack(line)
        return newLineSet(lines, buffer)
      } else {
        if (buffer.length() > 0)
          buffer.append('\n')
        buffer.append(line)
      }
    }
    if (buffer.length() > 0) newLineSet(lines, buffer)
    else null
  }

  @inline
  private def newLineSet(lines: SourceLineIterator, buffer: StringBuffer): LineSet = {
    val line = buffer.toString
    LineSet(Seq(line), line, lines.sourceRef)
  }

  override def canParse(firstLine: String): Boolean = firstLine.charAt(0) == '['

  override def parse(s: LineSet): Log4jEntry = Log4jEntry.fromString(s.fullText, s.source)
}
