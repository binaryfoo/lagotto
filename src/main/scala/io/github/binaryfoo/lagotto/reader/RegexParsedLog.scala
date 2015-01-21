package io.github.binaryfoo.lagotto.reader

import java.util.regex.Pattern

import io.github.binaryfoo.lagotto.{SimpleLogEntry, SourceRef}
import org.joda.time.format.DateTimeFormat

import scala.collection.mutable

/**
 * Assumes single line records.
 */
case class RegexParsedLog(pattern: String, timeFormat: String) extends LogType[SimpleLogEntry] {

  private val compiledPattern = Pattern.compile(pattern)
  private val groupNames = extractGroupNames(compiledPattern)
  private val dateTimeFormat = DateTimeFormat.forPattern(timeFormat)

  override def apply(lines: SourceLineIterator): SimpleLogEntry = {
    if (lines.hasNext)
      fromString(lines.next(), SourceRef(lines.sourceName, lines.lineNumber))
    else
      null
  }

  override def canParse(firstLine: String, fileName: String): Boolean = compiledPattern.matcher(firstLine).matches()

  def fromString(s: String, source: SourceRef = null): SimpleLogEntry = {
    val matcher = compiledPattern.matcher(s)
    if (!matcher.matches()) {
      throw new IllegalArgumentException(s"No match found in: '$s'")
    }
    val fields = mutable.LinkedHashMap[String, String]()
    groupNames.foreach { name =>
      fields.put(name, matcher.group(name))
    }
    SimpleLogEntry(fields, dateTimeFormat, s, source)
  }

  private def extractGroupNames(pattern: Pattern): Seq[String] = {
    import scala.collection.JavaConversions.asScalaSet

    val method = pattern.getClass.getDeclaredMethod("namedGroups")
    method.setAccessible(true)
    val nameToIndex = method.invoke(pattern).asInstanceOf[java.util.Map[String, Integer]].keySet()
    val nameSet: mutable.Set[String] = nameToIndex
    nameSet.toSeq
  }
}
