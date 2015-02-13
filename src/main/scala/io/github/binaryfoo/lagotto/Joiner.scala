package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.{AbstractIterator, mutable}

/**
 * Performs a full outer join of log entries on ``field``. Output lines will be joined using ``delimiter``.
 */
class Joiner(val field: FieldExpr, val delimiter: Char) extends (LogEntry => Option[JoinedEntry]) {

  private val pending = mutable.LinkedHashMap[String, LogEntry]()

  override def apply(e: LogEntry): Option[JoinedEntry] = {
    val key = field(e)
    pending.remove(key).map(JoinedEntry(_, e, field, delimiter)).orElse {
      pending.put(key, e)
      None
    }
  }

  def outerJoin(it: Iterator[LogEntry]): Iterator[JoinedEntry] = {
    val joined = it.flatMap(this.apply)
    new AbstractIterator[JoinedEntry] {

      private lazy val leftovers = pending.values.map(JoinedEntry(_, LogEntry.empty, field, delimiter)).iterator
      private def current = if (joined.hasNext) joined else leftovers

      override def next(): JoinedEntry = current.next()
      override def hasNext: Boolean = current.hasNext

    }
  }

  def innerJoin(it: Iterator[LogEntry]): Iterator[JoinedEntry] = it.flatMap(this.apply)
}

case class JoinedEntry(left: LogEntry, right: LogEntry, join: FieldExpr, delimiter: Char) extends LogEntry {

  def apply(field: String): String = {
    field match {
      case "rtt" => rtt.toString
      case JoinedEntryFieldAccess.Left(_, f) => left(f)
      case JoinedEntryFieldAccess.Right(_, f) => right(f)
      case _ =>
        val v = left(field)
        if (v == null) right(field) else v
    }
  }

  def rtt: Long = right.timestamp.getMillis - left.timestamp.getMillis

  def timestamp: DateTime = left.timestamp

  override def lines: String = {
    left.lines + delimiter + excludeJoinField(delimiter, right).mkString(delimiter.toString)
  }

  private def excludeJoinField(d: Char, r: LogEntry) = {
    r.exportAsSeq.collect { case (field, value) if field != join.field => value}
  }

  override def exportAsSeq: Seq[(String, String)] = left.exportAsSeq ++ right.exportAsSeq
}


object JoinedEntryFieldAccess {

  val Left = """(left)\.(.*)""".r
  val Right = """(right)\.(.*)""".r

  def unapply(expr: String): Option[(String, String)] = expr match {
    case Left(p, f) => Some((p, f))
    case Right(p, f) => Some((p, f))
    case _ => None
  }
}

object JoinMode extends Enumeration {
  type JoinMode = Value
  val Outer, Inner = Value
}

