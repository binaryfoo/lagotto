package io.github.binaryfoo.lagotto.reader

import java.io.File

import io.github.binaryfoo.gclog._
import io.github.binaryfoo.lagotto.{FileRef, SourceRef, GcLogEntry}

import scala.collection.mutable

/**
 * Garbage collection log (-verbose:gc)
 */
class JvmGCLog extends LogType[GcLogEntry] {

  type P = PreParsed[GcLogEntry]

  private var calculationFile: File = null
  private var rateCalculator: RateCalculator = null

  private def calculatorFor(sourceRef: SourceRef): RateCalculator = {
    sourceRef match {
      case FileRef(file, _) =>
        if (file != calculationFile) {
          rateCalculator = new RateCalculator
          calculationFile = file
        }
      case _ if rateCalculator == null =>
        rateCalculator = new RateCalculator
    }
    rateCalculator
  }

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
          val calculator = calculatorFor(sourceRef)
          val eventWithRates = calculator.addRates(event)
          return PreParsed(GcLogEntry(event.time, mutable.LinkedHashMap(eventWithRates.toSeq :_*), attempt, sourceRef), sourceRef)
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
