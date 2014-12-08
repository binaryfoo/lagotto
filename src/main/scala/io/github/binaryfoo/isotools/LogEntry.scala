package io.github.binaryfoo.isotools

import org.joda.time.DateTime

import scala.collection.mutable

/**
 * A single &lt;log&gt; entry from a jPOS log.
 *
 * @param fields Output of parser
 * @param lines Full text of the entry
 * @param source Where the entry was read from
 */
case class LogEntry(fields: Map[String, String], lines: String = "", source: SourceRef = null) extends Coalesced with ConvertibleToMap {

  lazy val timestamp: DateTime = JposTimestamp.parse(at)

  def realm: String = fields.getOrElse("realm", "")

  def at: String = fields.getOrElse("at", "")

  def msgType: String = fields.getOrElse("msgType", "")

  def icon: String = msgType match {
    case "send" => "\u2192"
    case "receive" => "\u2190"
    case _ => msgType
  }

  def mti: String = field("0")

  def link: String = {
    val dot = realm.indexOf('.')
    if (dot == -1) realm else realm.substring(0, dot)
  }

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
        case "link" => link
        case "icon" => icon
        case "file" if source != null => source.toString
        case "line" if source != null => source.line.toString
        case _ => null
      }
    } else {
      v
    }
  }
}

object LogEntry {

  import io.github.binaryfoo.isotools.TagType._

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
      tagNameAndType(line) match {
        case ("field", Start) =>
          val (id, value) = extractIdAndValue(line)
          fields.put(pathTo(id), value)
        case ("isomsg", Start) =>
          val (_, id) = extractId(line)
          if (id != null)
            pushPath(id)
        case ("isomsg", End) if path.nonEmpty =>
          popPath()
        case ("log", Start) =>
          extractAttributes(line, fields)
        case (name, Start) if !msgTypeBlackList.contains(name) =>
          fields.put("msgType", name)
        case _ =>
      }
    }
    if (!fields.contains("at"))
      throw new IllegalArgumentException(s"Missing 'at' in $lines")

    if (!fields.contains("realm"))
      throw new IllegalArgumentException(s"Missing 'realm' in $lines")
    LogEntry(fields.toMap, lines.mkString("\n"), source)
  }

  val msgTypeBlackList = Set("field", "isomsg", "log", "!--")

  def tagNameAndType(line: String): (String, TagType) = {
    var startIndex = line.indexOf('<')
    if (startIndex == -1)
      return (null, null)

    val tagType = if (startIndex + 1 < line.length && line.charAt(startIndex + 1) == '/') {
      startIndex += 2
      End
    } else {
      startIndex += 1
      Start
    }
    val name = new mutable.StringBuilder()

    for (i <- startIndex to line.length - 1) {
      val c = line.charAt(i)
      if (c == ' ' || c == '>' || c == '/')
        return (name.toString(), tagType)
      else
        name.append(c)
    }

    (name.toString(), tagType)
  }

  // slightly faster than a regex
  def extractAttributes(line: String, fields: mutable.Map[String, String]) = {
    var state = -1
    var nameStart = 0
    var nameEnd = 0
    var valueStart = 0
    var i = 0

    for (c <- line) {
      state match {
        case -1 =>
          if (c == '<')
            state = 0
        case 0 =>
          if (c == ' ') {
            nameStart = i + 1
            state = 1
          }
        case 1 =>
          if (c == '=') {
            nameEnd = i
            state = 2
          }
        case 2 if c == '"' =>
          valueStart = i + 1
          state = 3
        case 3 =>
          if (c == '"') {
            val name = line.substring(nameStart, nameEnd)
            val value = line.substring(valueStart, i)
            fields.put(name, value)
            state = 0
          }
      }
      i += 1
    }
  }

  def extractIdAndValue(line: String): (String, String) = {
    val (idEnd: Int, id: String) = extractId(line)
    if (id == null)
      throw new IllegalArgumentException(s"Missing id in $line")

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
      (-1, null)
    } else {
      val idStart = idIndex + 4
      val idEnd = line.indexOf('"', idStart)
      val id = line.substring(idStart, idEnd)
      (idEnd, id)
    }
  }

  def apply(fields: (String, String)*): LogEntry = {
    LogEntry(fields.toMap)
  }

  def coalesce(seq: Stream[LogEntry], selector: LogEntry => String): Iterable[Coalesced] = Collapser.coalesce(seq, selector)
}

case class SourceRef(file: String, line: Int) {
  override def toString: String = s"$file:$line"
}

object TagType extends Enumeration {
  type TagType = Value
  val Start, End = Value
}