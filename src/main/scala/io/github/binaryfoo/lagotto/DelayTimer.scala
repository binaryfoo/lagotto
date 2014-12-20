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
}

object DelayTimer {
  def calculateDelays(s: Stream[LogLike]): Stream[DelayTimer] = {
    var previous: Option[LogLike] = None
    s.map { e =>
      val next = DelayTimer(e, previous)
      previous = Some(e)
      next
    }
  }

  implicit class RichLogLikeStream(val v: Stream[LogLike]) extends AnyVal {
    def withDelays(): Stream[LogLike] = calculateDelays(v)
  }
}
