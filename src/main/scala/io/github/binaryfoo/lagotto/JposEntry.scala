package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * A single &lt;log&gt; entry from a jPOS log.
 *
 * @param _fields Output of parser
 * @param lines Full text of the entry
 * @param source Where the entry was read from
 */
case class JposEntry(private val _fields: mutable.LinkedHashMap[String, String], lines: String = "", source: SourceRef = null) extends Coalesced with LogEntry {

  /**
   * The 'at' attribute parsed as a joda DateTime. Viewed as mandatory.
   *
   * @throws IllegalArgumentException If the record contains no 'at' attribute.
   */
  val timestamp: DateTime = _fields.get("at") match {
    case Some(v) => JposTimestamp.parse(v)
    case None => null
  }

  val fields = _fields.withDefault {
    case "mti" => mti
    case "link" => realm.link
    case "icon" => icon
    case "socket" => realm.socket
    case "ipAddress" => realm.ipAddress
    case "port" => realm.port
    case "src" if source != null => source.toString
    case "file" if source != null => source.file
    case "line" if source != null => source.line.toString
    case TimeFormatter(format) => format.print(timestamp)
    case _ => null
  }

  /**
   * Format like Joda with same extras like HH:mm:s0 for 10 second buckets.
   * @param pattern
   * @return Timestamp as string
   */
  def timestampAs(pattern: String): String = timestampAs(new HumanTimeFormatter(pattern))

  def timestampAs(format: TimeFormatter): String = format.print(timestamp)

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

  def millisSince(e: JposEntry): Long = timestamp.getMillis - e.timestamp.getMillis

  def lifespan: Option[Int] = fields.get("lifespan").map {
    case s: String => s.toInt
  }

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

object JposEntry {

  import io.github.binaryfoo.lagotto.TagType._

  def extractFields(lines: Seq[String]): mutable.LinkedHashMap[String, String] = {
    var fields = new ArrayBuffer[(String, String)](lines.size + 2)
    var path = ""
    var msgType: String = null

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

    val it = lines.iterator
    while (it.hasNext) {
      val line = it.next()
      tagNameAndType(line) match {
        case ("field", Start) =>
          val (id, value) = extractIdAndValue(line, it)
          fields += ((pathTo(id), value))
        case ("isomsg", Start) =>
          val (_, id) = extractId(line)
          if (id != null)
            pushPath(id)
        case ("isomsg", End) if path.nonEmpty =>
          popPath()
        case ("log", Start) =>
          fields ++= extractAttributes(line).map {
            case (name, value) if name == "lifespan" => (name, value.replace("ms", ""))
            case a => a
          }
        case ("exception", Start) =>
          extractAttributes(line)
            .collectFirst { case ("name", value) => value }
            .foreach(v => fields += (("exception", v)))
        case (name, Start) if !msgTypeBlackList.contains(name) && msgType == null =>
          fields += (("msgType", name))
          msgType = name
        case _ =>
      }
    }

    mutable.LinkedHashMap(fields :_*)
  }
  
  def fromLines(lines: Seq[String], source: SourceRef = null): JposEntry = {
    JposEntry(extractFields(lines), lines.mkString("\n"), source)
  }

  def fromString(s: String, source: SourceRef = null): JposEntry = {
    JposEntry(extractFields(s.split('\n')), s, source)
  }

  val msgTypeBlackList = Set("log", "!--")

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

    (findTag(line, startIndex, startIndex + 1), tagType)
  }

  @tailrec
  private def findTag(line: String, from: Int, i: Int): String = {
    if (i < line.length) {
      val c = line.charAt(i)
      if (c == ' ' || c == '>' || c == '/')
        line.substring(from, i)
      else
        findTag(line, from, i + 1)
    } else {
      line.substring(from)
    }
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

  private val CDataTag = "![CDATA["

  def extractIdAndValue(line: String, more: Iterator[String]): (String, String) = {
    val (idEnd: Int, id: String) = extractId(line)
    if (id == null)
      throw new IllegalArgumentException(s"Missing id in $line")

    val valueIndex = line.indexOf("value=\"", idEnd)
    if (valueIndex != -1) {
      val valueStart = valueIndex + 7
      val valueEnd = line.indexOf('"', valueStart)
      val value = line.substring(valueStart, valueEnd)
      (id, value)
    } else {
      val cdataIndex = line.indexOf(CDataTag)
      if (cdataIndex != -1) {
        val cdata = new StringBuilder()
        readCData(line.substring(cdataIndex + CDataTag.length), more, cdata)
        (id, cdata.toString())
      } else {
        throw new IllegalArgumentException(s"Missing value in $line")
      }
    }
  }

  @tailrec
  private def readCData(current: String, lines: Iterator[String], cdata: StringBuilder): Unit = {
    if (cdata.nonEmpty)
      cdata.append('\n')
    val endCdataIndex = current.indexOf("]]>")
    if (endCdataIndex == -1) {
      cdata.append(current)
      if (lines.hasNext) {
        readCData(lines.next(), lines, cdata)
      }
    } else {
      cdata.append(current.substring(0, endCdataIndex))
    }
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

  def apply(fields: (String, String)*): JposEntry = {
    JposEntry(mutable.LinkedHashMap(fields :_*))
  }

  def coalesce(seq: Iterator[JposEntry], selector: JposEntry => String): Iterator[Coalesced] = Collapser.coalesce(seq, selector)
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

  override def toString: String = raw
}
