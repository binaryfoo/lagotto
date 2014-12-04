package io.github.binaryfoo.isotools

import java.util.regex.Pattern

import io.github.binaryfoo.isotools.Iso8583.{normaliseToRequestMTI, isResponseMTI}
import org.joda.time.DateTime

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

case class LogEntry(fields: Map[String, String]) extends Coalesced with ConvertibleToMap {
  lazy val timestamp: DateTime = JposTimestamp.parse(at)

  def realm: String = fields.getOrElse("realm", "")

  def at: String = fields.getOrElse("at", "")

  def mti: String = field("0")

  def field(path: String): String = fields.getOrElse(path, null)

  def hasField(path: String): Boolean = fields.contains(path)

  def apply(path: String): String = {
    val v = field(path)
    if (v == null) {
      path match {
        case "realm" => realm
        case "at" => at
        case "mti" => mti
        case "timestamp" => timestamp.toString("yyyy-MM-dd HH:mm:ss.SSS")
        case "time" => timestamp.toString("HH:mm:ss.SSS")
        case "date" => timestamp.toString("yyyy-MM-dd")
        case _ => null
      }
    } else {
      v
    }
  }
}

object LogEntry {

  val ID_VAL_PATTERN = Pattern.compile("(\\w+)=\"([^\"]*)\"")

  def fromLines(lines: Iterable[String]): LogEntry = {
    val fields = new mutable.ListMap[String, String]
    val path = new ListBuffer[String]()

    def pathTo(last: String) = (path.toSeq :+ last) mkString "."

    for (line <- lines) {
      val attributes = extractAttributes(line)
      if (line.contains("<field")) {
        val id = attributes("id")
        if (id == null)
          throw new IllegalArgumentException(s"Missing id in field $line")
        val value = attributes("value")
        if (value == null)
          throw new IllegalArgumentException(s"Missing value in field $line")
        fields.put(pathTo(id), value)
      } else if (line.contains("<isomsg ") || line.contains("<isomsg>")) {
        attributes.get("id").map(path += _)
      } else if (line.contains("</isomsg>") && path.nonEmpty) {
        path.remove(path.size - 1)
      } else if (line.contains("<log ")) {
        attributes.foreach(fields += _)
      }
    }
    LogEntry(fields.toMap)
  }

  def extractAttributes(line: String): Map[String, String] = {
    val attrs = new mutable.ListMap[String, String]
    val matcher = ID_VAL_PATTERN.matcher(line)
    while (matcher.find()) {
      val name = matcher.group(1)
      val value = matcher.group(2)
      attrs.put(name, value)
    }
    attrs.toMap
  }

  def apply(fields: (String, String)*): LogEntry = {
    new LogEntry(Map(fields : _*))
  }

  def coalesce(seq: Iterable[LogEntry], selector: LogEntry => String): Iterable[Coalesced] = Collapser.coalesce(seq, selector)
}
