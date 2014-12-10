package io.github.binaryfoo.isotools

import org.joda.time.DateTime

/**
 * A single &lt;log&gt; entry from a jPOS log.
 *
 * @param fields Output of parser
 * @param lines Full text of the entry
 * @param source Where the entry was read from
 */
case class LogEntry(fields: Map[String, String], lines: String = "", source: SourceRef = null) extends Coalesced with ConvertibleToMap {

  lazy val timestamp: DateTime = {
    if (at == "")
      throw new IllegalArgumentException(s"Missing 'at' in $lines")

    JposTimestamp.parse(at)
  }

  def realm: Realm = Realm(fields.getOrElse("realm", ""))

  def at: String = fields.getOrElse("at", "")

  def msgType: String = fields.getOrElse("msgType", "")

  def icon: String = msgType match {
    case "send" => "\u2192"
    case "receive" => "\u2190"
    case _ => msgType
  }

  def mti: String = field("0")

  def field(path: String): String = fields.getOrElse(path, null)

  def hasField(path: String): Boolean = fields.contains(path)

  def apply(path: String): String = {
    val v = field(path)
    if (v == null) {
      path match {
        case "realm" => realm.raw
        case "at" => at
        case "mti" => mti
        case "timestamp" => timestamp.toString("yyyy-MM-dd HH:mm:ss.SSS")
        case "time" => timestamp.toString("HH:mm:ss.SSS")
        case "date" => timestamp.toString("yyyy-MM-dd")
        case "link" => realm.link
        case "icon" => icon
        case "socket" => realm.socket
        case "ipAddress" => realm.ipAddress
        case "port" => realm.port
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

  def extractFields(lines: Seq[String]): Map[String, String] = {
    var fields = List[(String, String)]()
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
          fields = (pathTo(id), value) :: fields
        case ("isomsg", Start) =>
          val (_, id) = extractId(line)
          if (id != null)
            pushPath(id)
        case ("isomsg", End) if path.nonEmpty =>
          popPath()
        case ("log", Start) =>
          fields = extractAttributes(line) ::: fields
        case (name, Start) if !msgTypeBlackList.contains(name) =>
          fields = ("msgType", name) :: fields
        case _ =>
      }
    }

    fields.toMap
  }
  
  def fromLines(lines: Seq[String], source: SourceRef = null): LogEntry = {
    LogEntry(extractFields(lines), lines.mkString("\n"), source)
  }

  def fromString(s: String, source: SourceRef = null): LogEntry = {
    LogEntry(extractFields(s.split('\n')), s, source)
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

    for (i <- startIndex to line.length - 1) {
      val c = line.charAt(i)
      if (c == ' ' || c == '>' || c == '/')
        return (line.substring(startIndex, i), tagType)
    }

    (line.substring(startIndex), tagType)
  }

  // slightly faster than a regex
  def extractAttributes(line: String): List[(String, String)] = {
    var state = -1
    var nameStart = 0
    var nameEnd = 0
    var valueStart = 0
    var i = 0
    var fields = List[(String, String)]()

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
            fields = (name, value) :: fields
            state = 0
          }
      }
      i += 1
    }

    fields
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

/**
 * Pull apart a 'realm' attribute from a &ltlog&gt; record.
 */
case class Realm(raw: String) {

  def link: String = {
    val fullLink = linkAndSocket._1
    // strip off .server or .channel added by jpos
    fullLink.split('.') match {
      case Array(link, _) => link
      case _ => fullLink
    }
  }

  def linkAndSocket: (String, String) = {
    raw.split('/') match {
      case Array(link, socket) => (link, socket)
      case _ => ("", "")
    }
  }

  def socket: String = linkAndSocket._2

  def ipAddress: String = {
    socket.split(':') match {
      case Array(ip, _) => ip
      case _ => socket
    }
  }

  def port: String = {
    socket.split(':') match {
      case Array(_, port) => port
      case _ => ""
    }
  }
}