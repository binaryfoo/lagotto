package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.Log4jEntry

/**
 * Log4j records. Assumes they start with [ and can span multiple lines.
 */
object Log4jLog extends LogType[Log4jEntry] {

  type P = TextAndSource

  override def readLinesForNextRecord(lines: LineIterator): TextAndSource = {
    val buffer = new StringBuffer()
    while (lines.hasNext) {
      if (buffer.length() > 0 && isNewRecord(lines.head)) {
        return newLineSet(lines, buffer)
      } else {
        if (buffer.length() > 0)
          buffer.append('\n')
        buffer.append(lines.next())
      }
    }
    if (buffer.length() > 0) newLineSet(lines, buffer)
    else null
  }

  private def isNewRecord(line: String): Boolean = line.startsWith("[") || line.contains("<log")

  @inline
  private def newLineSet(lines: LineIterator, buffer: StringBuffer): TextAndSource = {
    TextAndSource(buffer.toString, lines.sourceRef)
  }

  override def canParse(firstLine: String): Boolean = firstLine.nonEmpty && firstLine.charAt(0) == '['

  override def parse(s: TextAndSource): Log4jEntry = Log4jEntry.fromString(s.text, s.source)
}
