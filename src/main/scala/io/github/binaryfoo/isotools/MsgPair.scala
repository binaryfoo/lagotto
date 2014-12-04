package io.github.binaryfoo.isotools

import io.github.binaryfoo.isotools.Iso8583._

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class MsgPair(request: LogEntry, response: LogEntry) {

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

}

object MsgPair {

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

}
