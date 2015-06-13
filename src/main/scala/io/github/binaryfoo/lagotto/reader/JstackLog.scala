package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.SimpleLogEntry

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * Parse the output of the jstack tool from the JDK into a LogEntry per Thread.
 */
object JstackLog extends LogType[SimpleLogEntry] {

  val FirstLine = """"([^"]+)".*""".r
  val SecondLine = """ *java.lang.Thread.State: (\w+).*""".r

  override type P = LineSet

  override def readLinesForNextRecord(it: LineIterator): LineSet = {
    var record: ListBuffer[String] = null
    for (line <- it) {
      line match {
        case FirstLine(_) if record == null =>
          record = ListBuffer(line)
        case _ if line.isEmpty && record != null =>
          return LineSet(record, record.mkString("\n"), it.sourceRef)
        case _ if record != null =>
          record += line
        case _ =>
      }
    }
    if (record != null)
      LineSet(record, record.mkString("\n"), it.sourceRef)
    else
      null
  }

  override def parse(s: LineSet): SimpleLogEntry = {
    val threadName = s.lines.head match {
      case FirstLine(name) => name
    }
    val contents = mutable.LinkedHashMap("name" -> threadName, "state" -> threadState(s))
    SimpleLogEntry(contents, lines = s.fullText)
  }

  def threadState(s: LineSet): String = {
    if (s.lines.length > 1) {
      s.lines(1) match {
        case SecondLine(state) => state
        case _ => stateFromFirstLine(s.lines.head)
      }
    } else  {
      stateFromFirstLine(s.lines.head)
    }
  }

  def stateFromFirstLine(line: String) = {
    if (line.contains("runnable")) {
      "RUNNABLE"
    } else {
      "WAITING"
    }
  }
}
