package io.github.binaryfoo.isotools

import java.util.regex.Pattern

import org.joda.time.DateTime

import scala.collection.mutable

case class LogEntry(fields: Map[String, String], lines: String = "", source: SourceRef = null) extends Coalesced with ConvertibleToMap {

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
        case "file" if source != null => source.toString
        case _ => null
      }
    } else {
      v
    }
  }
}

object LogEntry {

  val ID_VAL_PATTERN = Pattern.compile("(\\w+)=\"([^\"]*)\"")

  def fromLines(lines: Seq[String], source: SourceRef = null): LogEntry = {
    val fields = new mutable.ListMap[String, String]
    var path = ""

    def pathTo(id: String) = if (path.isEmpty) id else path + "." + id

    def pushPath(id: String) = {
      if (path.isEmpty)
        path = id
      else
        path += "." + id
    }

    def popPath() = {
      val dot = path.lastIndexOf('.')
      if (dot == -1)
        path = ""
      else
        path = path.substring(0, dot)
    }

    for (line <- lines) {
      if (line.contains("<field")) {
        val (id, value) = extractIdAndValue(line)
        fields.put(pathTo(id), value)
      } else if (line.contains("<isomsg id")) {
        val (_, id) = extractId(line)
        pushPath(id)
      } else if (line.contains("</isomsg>") && path.nonEmpty) {
        popPath()
      } else if (line.contains("<log ")) {
        extractAttributes(line).foreach(fields += _)
      }
    }
    LogEntry(fields.toMap, lines.mkString("\n"), source)
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

  def extractIdAndValue(line: String): (String, String) = {
    val (idEnd: Int, id: String) = extractId(line)

    val valueIndex = line.indexOf("value=\"", idEnd)
    if (valueIndex == -1) {
      throw new IllegalArgumentException(s"Missing value in $line")
    }
    val valueStart = valueIndex + 7
    val valueEnd = line.indexOf('"', valueStart)
    val value = line.substring(valueStart, valueEnd)

    (id, value)
  }

  def extractId(line: String): (Int, String) = {
    val idIndex = line.indexOf("id=\"")
    if (idIndex == -1) {
      throw new IllegalArgumentException(s"Missing id in $line")
    }
    val idStart = idIndex + 4
    val idEnd = line.indexOf('"', idStart)
    val id = line.substring(idStart, idEnd)

    (idEnd, id)
  }

  def apply(fields: (String, String)*): LogEntry = {
    LogEntry(fields.toMap)
  }

  def coalesce(seq: Stream[LogEntry], selector: LogEntry => String): Iterable[Coalesced] = Collapser.coalesce(seq, selector)
}

case class SourceRef(file: String, line: Int) {
  override def toString: String = s"$file:$line"
}
