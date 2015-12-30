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
    val buffer = mutable.ArrayBuffer[String]()
    var firstLine = -1
    while (lines.hasNext) {
      val line = lines.next()
      if (buffer.isEmpty)
        firstLine = lines.lineNumber
      buffer.append(line)
      val attemptedLines = buffer.mkString("\n")
      Parser.incrementalParse(attemptedLines) match {
        case SkipLine =>
          buffer.remove(buffer.size - 1)
        case GcEventParsed(event) =>
          val sourceRef = lines.sourceRef.at(firstLine)
          return PreParsed(GcLogEntry(event.time, mutable.LinkedHashMap(event.toSeq.sorted :_*), attemptedLines, sourceRef), sourceRef)
        case NeedAnotherLine =>
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
