package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.Log4jEntry

/**
 * Log4j records. Assumes they start with [ and can span multiple lines.
 */
object Log4jLog extends LogType[Log4jEntry] {

  type P = TextAndSource

  override def readLinesForNextRecord(lines: SourceLineIterator): TextAndSource = {
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
  private def newLineSet(lines: SourceLineIterator, buffer: StringBuffer): TextAndSource = {
    TextAndSource(buffer.toString, lines.sourceRef)
  }

  override def canParse(firstLine: String): Boolean = firstLine.charAt(0) == '['

  override def parse(s: TextAndSource): Log4jEntry = Log4jEntry.fromString(s.text, s.source)
}
