package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

case class DelayTimer(current: LogLike, previous: Option[LogLike]) extends LogLike {

  override def timestamp: DateTime = current.timestamp

  override def apply(id: String): String = id match {
    case "delay" => delay.getOrElse(0).toString
    case _ => current(id)
  }

  override def lines: String = current.lines

  def delay: Option[Long] = previous.map { p =>
    current.timestamp.getMillis - p.timestamp.getMillis
  }

  override def toMap: Map[String, String] = current.toMap + ("delay" -> delay.map(_.toString).getOrElse(""))
}
