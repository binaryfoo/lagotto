package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

/**
 * Calculates the delay between two log entries.
 */
case class DelayTimer(current: LogEntry, previous: Option[LogEntry]) extends LogEntry {

  override def timestamp: DateTime = current.timestamp
  override def source: SourceRef = current.source
  override def lines: String = current.lines

  override def apply(id: String): String = id match {
    case "delay" => delay.getOrElse(0).toString
    case _ => current(id)
  }

  def delay: Option[Long] = previous.map { p =>
    current.timestamp.getMillis - p.timestamp.getMillis
  }

  override def exportAsSeq: Seq[(String, String)] = current.exportAsSeq :+ ("delay", delay.map(_.toString).getOrElse(""))
}
