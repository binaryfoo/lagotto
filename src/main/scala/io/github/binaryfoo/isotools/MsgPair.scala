package io.github.binaryfoo.isotools

import io.github.binaryfoo.isotools.Iso8583._
import org.joda.time.DateTime

import scala.collection.mutable

/**
 * A single request paired with its response. Eg an auth (0200) and reply (0210).
 */
case class MsgPair(request: LogEntry, response: LogEntry) extends Coalesced with ConvertibleToMap {

  val Request = """(req|request)\.(.*)""".r
  val Response = """(resp|response)\.(.*)""".r

  def apply(field: String): String = {
    field match {
      case "rtt" => rtt.toString
      case Request(_, f) => request(f)
      case Response(_, f) => response(f)
      case _ =>
        val v = request(field)
        if (v == null) response(field) else v
    }
  }

  def rtt: Long = response.timestamp.getMillis - request.timestamp.getMillis

  def timestamp: DateTime = request.timestamp

  def mti: String = this("mti")

  override def toString: String = s"Pair(req=${request.fields.mkString("{", ",", "}")},resp=${response.fields.mkString("{", ",", "}")})"

  override lazy val lines: Seq[String] = Seq("<pair>") ++ request.lines ++ response.lines ++ Seq("</pair>")
}

object MsgPair {

  def coalesce(seq: Stream[MsgPair], selector: MsgPair => String): Iterable[Coalesced] = Collapser.coalesce(seq, selector)

  def pair(list: Stream[LogEntry]): Stream[MsgPair] = {
    val pending = new mutable.ListMap[String, LogEntry]

    def pairNext(s: Stream[LogEntry]): Stream[MsgPair] = {
      s match {
        case e #:: tail =>
          val mti = e.mti
          if (mti != null) {
            val key = normaliseToRequestMTI(mti) + "-" + e("11").toInt
            pending.get(key) match {
              case Some(other) =>
                val m = if (isResponseMTI(mti)) new MsgPair(other, e) else new MsgPair(e, other)
                pending.remove(key)
                return m #:: pairNext(tail)
              case None => pending.put(key, e)
            }
          }
          pairNext(tail)
        case _ => Stream.empty
      }
    }

    pairNext(list)
  }

  implicit class RichEntryIterable(val v: Stream[LogEntry]) extends AnyVal {
    def pair(): Stream[MsgPair] = MsgPair.pair(v)
  }

  implicit class RichMsgPairIterable(val v: Stream[MsgPair]) extends AnyVal {
    def coalesce(selector: MsgPair => String): Stream[Coalesced] = Collapser.coalesce(v, selector)
  }
}