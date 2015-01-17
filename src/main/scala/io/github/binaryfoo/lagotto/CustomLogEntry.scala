package io.github.binaryfoo.lagotto

import java.util.regex.Pattern

import org.joda.time.DateTime
import org.joda.time.format.{DateTimeFormatter, DateTimeFormat}

import scala.collection.mutable

case class CustomLogEntry(private val _fields: mutable.LinkedHashMap[String, String], private val timeFormat: DateTimeFormatter, lines: String, source: SourceRef = null) extends LogEntry {

  val fields = _fields.withDefault {
    case TimeFormatter(format) => format.print(timestamp)
    case _ => null
  }

  lazy val timestamp: DateTime = {
    _fields.get("timestamp")
      .map(timeFormat.parseDateTime)
      .getOrElse(throw new IAmSorryDave(s"Missing 'timestamp' in ${_fields}"))
  }

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

class CustomLogEntryParser(val pattern: String, val timeFormat: String) {

  private val compiledPattern = Pattern.compile(pattern)
  private val groupNames = extractGroupNames(compiledPattern)
  private val dateTimeFormat = DateTimeFormat.forPattern(timeFormat)

  def fromString(s: String, source: SourceRef = null): CustomLogEntry = {
    val matcher = compiledPattern.matcher(s)
    if (!matcher.matches()) {
      throw new IllegalArgumentException(s"No match found in: '$s'")
    }
    val fields = mutable.LinkedHashMap[String, String]()
    groupNames.foreach { name =>
      fields.put(name, matcher.group(name))
    }
    CustomLogEntry(fields, dateTimeFormat, s, source)
  }

  def extractGroupNames(pattern: Pattern): Seq[String] = {
    import scala.collection.JavaConversions.asScalaSet

    val method = pattern.getClass.getDeclaredMethod("namedGroups")
    method.setAccessible(true)
    val nameToIndex = method.invoke(pattern).asInstanceOf[java.util.Map[String, Integer]].keySet()
    val nameSet: mutable.Set[String] = nameToIndex
    nameSet.toSeq
  }
}