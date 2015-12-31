package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.gclog.{GcEventParsed, NeedAnotherLine, Parser, SkipLine}
import io.github.binaryfoo.lagotto.GcLogEntry

import scala.collection.mutable

/**
 * Garbage collection log (-verbose:gc)
 */
object JvmGCLog extends LogType[GcLogEntry] {

  type P = PreParsed[GcLogEntry]

  override def readLinesForNextRecord(lines: LineIterator): PreParsed[GcLogEntry] = {
    var lastAttempt: String = null
    var firstLine = -1
    while (lines.hasNext) {
      val line = lines.next()
      val attempt = if (lastAttempt == null) {
        firstLine = lines.lineNumber
        line
      } else {
        lastAttempt + "\n" + line
      }
      Parser.incrementalParse(attempt) match {
        case SkipLine =>
        case GcEventParsed(event) =>
          val sourceRef = lines.sourceRef.at(firstLine)
          return PreParsed(GcLogEntry(event.time, mutable.LinkedHashMap(event.toSeq :_*), attempt, sourceRef), sourceRef)
        case NeedAnotherLine =>
          lastAttempt = attempt
      }
    }
    null
  }

  override def canParse(firstLine: String): Boolean = Parser.incrementalParse(firstLine) match {
    case SkipLine => false
    case _ => true
  }

  override def parse(s: PreParsed[GcLogEntry]): GcLogEntry = s.entry
}
