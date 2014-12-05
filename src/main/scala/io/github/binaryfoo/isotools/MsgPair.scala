package io.github.binaryfoo.isotools

import io.github.binaryfoo.isotools.Iso8583._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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

  def mti: String = this("mti")

  override def toString: String = s"Pair(req=${request.fields.mkString("{", ",", "}")},resp=${response.fields.mkString("{", ",", "}")})"
}

object MsgPair {

  def coalesce(seq: Iterable[MsgPair], selector: MsgPair => String): Iterable[Coalesced] = Collapser.coalesce(seq, selector)

  def pair(list: Iterable[LogEntry]): Iterable[MsgPair] = {
    val pending = new mutable.ListMap[String, LogEntry]
    val matches = new ListBuffer[MsgPair]()

    for (e <- list) {
      val mti = e.mti
      if (mti != null) {
        val key = normaliseToRequestMTI(mti) + "-" + e("11").toInt
        pending.get(key) match {
          case Some(other) =>
            matches += (if (isResponseMTI(mti)) new MsgPair(other, e) else new MsgPair(e, other))
            pending.remove(key)
          case None => pending.put(key, e)
        }
      }
    }
    matches
  }

  implicit class RichEntryIterable(val v: Iterable[LogEntry]) extends AnyVal {
    def pair(): Iterable[MsgPair] = MsgPair.pair(v)
  }

  implicit class RichMsgPairIterable(val v: Iterable[MsgPair]) extends AnyVal {
    def coalesce(selector: MsgPair => String): Iterable[Coalesced] = Collapser.coalesce(v, selector)
  }
}