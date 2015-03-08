package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.Iso8583._
import org.joda.time.DateTime

import scala.collection.mutable

/**
 * A single request paired with its response. Eg an auth (0200) and reply (0210).
 */
case class MsgPair(request: JposEntry, response: JposEntry) extends Coalesced with LogEntry {

  def apply(field: String): String = {
    field match {
      case "rtt" => rtt.toString
      case _ =>
        val v = request(field)
        if (v == null) response(field) else v
    }
  }

  def rtt: Long = response.timestamp.getMillis - request.timestamp.getMillis

  def timestamp: DateTime = request.timestamp
  def source: SourceRef = request.source
  def mti: String = this("mti")

  override def toString: String = s"Pair(req=${request.fields.mkString("{", ",", "}")},resp=${response.fields.mkString("{", ",", "}")})"

  override lazy val lines: String = "<pair>\n" + request.lines + "\n" + response.lines + "\n</pair>"

  override def exportAsSeq: Seq[(String, String)] = request.exportAsSeq ++ response.exportAsSeq
}

/**
 * Function to match requests with responses based on MTI, STAN (field 11) and realm.
 */
class MsgPairing extends (JposEntry => Option[MsgPair]) {

  private val pending = new mutable.LinkedHashMap[String, JposEntry]

  /**
   * If e creates a (request, response) pair return the pair. Otherwise remember e for a future match.
   */
  override def apply(e: JposEntry): Option[MsgPair] = {
    val mti = e.mti
    if (mti != null) {
      val partnersKey = key(invertMTI(mti), e)
      pending.get(partnersKey) match {
        case Some(other) =>
          val m = if (isResponseMTI(mti)) MsgPair(other, e) else MsgPair(e, other)
          pending.remove(partnersKey)
          Some(m)
        case None =>
          val thisKey = key(mti, e)
          pending.put(thisKey, e)
          None
      }
    } else {
      None
    }
  }

  private def key(mti: String, e: JposEntry): String = mti + "-" + toIntIfPossible(e("11"))   + "-" + e.realm.raw

  private def toIntIfPossible(s: String): Any = {
    try {
      s.toInt
    }
    catch {
      case e: NumberFormatException => s
    }
  }
}

object MsgPair {

  def coalesce(seq: Iterator[MsgPair], selector: MsgPair => String): Iterator[Coalesced] = Collapser.coalesce(seq, selector)

  /**
   * Match requests with responses based on MTI, STAN (field 11) and realm.
   */
  def pair(list: Iterator[JposEntry]): Iterator[MsgPair] = {
    list.flatMap(new MsgPairing().apply)
  }

  implicit class RichEntryIterable(val v: Iterator[JposEntry]) extends AnyVal {
    def pair(): Iterator[MsgPair] = MsgPair.pair(v)
  }

  implicit class RichMsgPairIterable(val v: Iterator[MsgPair]) extends AnyVal {
    def coalesce(selector: MsgPair => String): Iterator[Coalesced] = Collapser.coalesce(v, selector)
  }

}

object MsgPairFieldAccess {

  val Request = """(req|request)\.(.*)""".r
  val Response = """(resp|response)\.(.*)""".r

  def unapply(expr: String): Option[(String, String)] = expr match {
    case Request(p, f) => Some(("request", f))
    case Response(p, f) => Some(("response", f))
    case _ => None
  }
}