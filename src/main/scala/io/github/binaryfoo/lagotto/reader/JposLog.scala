package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.{LogEntry, SourceRef}

import scala.collection.mutable.ListBuffer

/**
 * XML output by jPOS' XMLPackager and logging framework.
 */
object JposLog extends LogType[LogEntry] {

  override def apply(lines: SourceLineIterator): LogEntry = {
    var record: ListBuffer[String] = null
    var startLineNumber = lines.lineNumber
    for (line <- lines) {
      if (line.contains("<log ")) {
        if (record == null) {
          startLineNumber = lines.lineNumber
          record = new ListBuffer[String]
        } else if (lines.strict) {
          throw new IllegalArgumentException(s"Unexpected <log> start tag. Line ${lines.sourceRef}: $line")
        }
      }

      if (record != null)
        record += line

      if (line.contains("</log>")) {
        if (record != null) {
          try {
            val fullText = if (lines.keepFullText) record.mkString("\n") else ""
            return LogEntry(LogEntry.extractFields(record), fullText, SourceRef(lines.sourceName, startLineNumber))
          }
          catch {
            case e: IllegalArgumentException =>
              if (lines.strict) {
                throw new IllegalArgumentException(s"Failed to process record ending line ${lines.sourceRef}", e)
              }
              record = null
          }

        } else if (lines.strict) {
          throw new IllegalArgumentException(s"Unexpected </log> end tag. Line ${lines.sourceRef}: $line")
        }
      }
    }

    if (record != null)
      throw new IllegalArgumentException(s"Unexpected end of input in ${lines.sourceRef}")
    else
      null
  }

}
