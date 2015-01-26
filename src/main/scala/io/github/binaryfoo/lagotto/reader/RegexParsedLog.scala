package io.github.binaryfoo.lagotto.reader

import java.util.regex.Pattern

import io.github.binaryfoo.lagotto.{SimpleLogEntry, SourceRef}
import org.joda.time.format.DateTimeFormat

import scala.collection.mutable

/**
 * Assumes single line records. Ignores lines not matched by lineRecogniser.
 */
case class RegexParsedLog(pattern: String, timeFormat: String, lineRecogniser: LineRecogniser = AnyLineRecogniser) extends LogType[SimpleLogEntry] {

  type P = TextAndSource

  private val compiledPattern = Pattern.compile(pattern)
  private val groupNames = extractGroupNames(compiledPattern)
  private val dateTimeFormat = DateTimeFormat.forPattern(timeFormat)

  override def canParse(firstLine: String): Boolean = compiledPattern.matcher(firstLine).matches()

  override def readLinesForNextRecord(it: SourceLineIterator): TextAndSource = {
    while (it.hasNext) {
      val line = it.next()
      if (lineRecogniser.isLogEntry(line))
        return TextAndSource(line, it.sourceRef)
    }
    null
  }

  override def parse(s: TextAndSource): SimpleLogEntry = fromString(s.text, s.source)

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

trait LineRecogniser {
  def isLogEntry(line: String): Boolean
}

object AnyLineRecogniser extends LineRecogniser {
  override def isLogEntry(line: String): Boolean = true
}

object GcLogLineRecogniser extends LineRecogniser {
  override def isLogEntry(line: String): Boolean = {
    line.length > 4 && line.substring(0, 4).forall(Character.isDigit) && line.contains("real=")
  }
}
