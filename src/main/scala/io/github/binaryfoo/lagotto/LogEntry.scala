package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

/**
 * A single &lt;log&gt; entry from a jPOS log.
 *
 * @param _fields Output of parser
 * @param lines Full text of the entry
 * @param source Where the entry was read from
 */
case class LogEntry(private val _fields: Map[String, String], lines: String = "", source: SourceRef = null) extends Coalesced with LogLike {

  val fields = _fields.withDefault {
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
    case LogEntry.TimeInFormat(format) => timestampAs(format)
    case LogEntry.XPathAccess(path) => xpath(path)
    case LogEntry.RegexReplacement(field, regex, replacement) =>
      val raw = this(field)
      if (raw != null) raw.replaceAll(regex, replacement) else null
    case _ => null
  }

  /**
   * The 'at' attribute parsed as a joda DateTime. Viewed as mandatory.
   *
   * @throws IllegalArgumentException If the record contains no 'at' attribute.
   */
  lazy val timestamp: DateTime = fields.get("at") match {
    case Some(v) => JposTimestamp.parse(v)
    case None => throw new IllegalArgumentException(s"Missing 'at' in $lines")
  }

  /**
   * Format like Joda with same extras like HH:mm:s0 for 10 second buckets.
   * @param format
   * @return Timestamp as string
   */
  def timestampAs(format: String): String = format match {
    case "HH:mm:s0" => timestamp.toString("HH:mm:ss").substring(0, 7) + "0"
    case "HH:m0" => timestamp.toString("HH:mm").substring(0, 4) + "0"
    case _ => timestamp.toString(format)
  }

  def xpath(path: String): String = XPathEval(lines, path)

  /**
   * The 'realm' attribute disassembled into parts.
   *
   * @return Never null. If missing a value containing empty strings.
   */
  def realm: Realm = fields.get("realm") match {
    case Some(v) => Realm(v)
    case None => Realm("")
  }

  def at: String = fields("at")

  /**
   * Values like "send", "receive", "session-start", "session-end".
   */
  def msgType: String = fields("msgType")

  def icon: String = msgType match {
    case "send" => "\u2192"
    case "receive" => "\u2190"
    case _ => msgType
  }

  def mti: String = fields("0")

  def apply(path: String): String = fields(path)

  def millisSince(e: LogEntry): Long = timestamp.getMillis - e.timestamp.getMillis

  def lifespan: Option[Int] = fields.get("lifespan").map {
    case s: String => s.toInt
  }
}

object LogEntry {

  import io.github.binaryfoo.lagotto.TagType._

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
          fields = extractAttributes(line).map {
            case (name, value) if name == "lifespan" => (name, value.replace("ms", ""))
            case a => a
          } ::: fields
        case ("exception", Start) =>
          val ("name", value) :: _ = extractAttributes(line)
          fields = ("exception", value) :: fields
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

  val TimeInFormat = """time\((.*)\)""".r
  val XPathAccess = """xpath\((.+)\)""".r
  val RegexReplacement = """([^(]+)\(/(.+)/(.*)/\)""".r
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
      case Array(link, _*) => link
      case _ => fullLink
    }
  }

  /**
   * Split the realm on slash to get two parts: the 'link' and ip-address:port.
   */
  def linkAndSocket: (String, String) = {
    raw.split('/') match {
      case Array(link, socket) => (link, socket)
      case _ => ("", "")
    }
  }

  /**
   * The ip-address:port pair from the realm.
   */
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
