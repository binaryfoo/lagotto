package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.Log4jEntry

/**
 * Log4j records. Assumes they start with [ and can span multiple lines.
 */
object Log4jLog extends LogType[Log4jEntry] {

  type P = TextAndSource

  override def readLinesForNextRecord(lines: LineIterator): TextAndSource = {
    val buffer = new StringBuffer()
    var firstLine = -1
    while (lines.hasNext) {
      if (buffer.length() > 0 && isNewRecord(lines.head)) {
        return newLineSet(lines, buffer, firstLine)
      } else {
        val line = lines.next()
        if (buffer.length() > 0)
          buffer.append('\n')
        else
          firstLine = lines.lineNumber
        buffer.append(line)
      }
    }
    if (buffer.length() > 0) newLineSet(lines, buffer, firstLine)
    else null
  }

  private def isNewRecord(line: String): Boolean = line.startsWith("[") || line.contains("<log")

  @inline
  private def newLineSet(lines: LineIterator, buffer: StringBuffer, firstLine: Int): TextAndSource = {
    TextAndSource(buffer.toString, lines.sourceRef.at(firstLine))
  }

  override def canParse(firstLine: String): Boolean = firstLine.nonEmpty && firstLine.charAt(0) == '['

  override def parse(s: TextAndSource): Log4jEntry = Log4jEntry.fromString(s.text, s.source)
}
