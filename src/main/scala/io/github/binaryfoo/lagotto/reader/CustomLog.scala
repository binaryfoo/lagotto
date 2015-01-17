package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.{CustomLogEntry, CustomLogEntryParser, SourceRef}

/**
 * Assumes records start with [.
 */
class CustomLog(val parser: CustomLogEntryParser) extends LogType[CustomLogEntry] {

  override def apply(lines: SourceLineIterator): CustomLogEntry = {
    if (lines.hasNext)
      parser.fromString(lines.next(), SourceRef(lines.sourceName, lines.lineNumber))
    else
      null
  }

}
