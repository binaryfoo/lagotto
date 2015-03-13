package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.dictionary.{FieldType, DataDictionary}
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType
import io.github.binaryfoo.lagotto.output.GZip
import io.github.binaryfoo.lagotto.shell.{PlainText, ContentType, OutputFormat}
import io.github.binaryfoo.lagotto.{DefaultDateTimeFormat, JposEntry, LogEntry}

case class DigestedFormat(dictionary: DataDictionary, nameType: Option[NameType]) extends OutputFormat {

  override def header(): Option[String] = None

  override def footer(): Option[String] = None

  override def apply(e: LogEntry): Option[String] = {
    Some(e match {
      case l: JposEntry => format(l)
      case _ => e.lines
    })
  }

  override val contentType: ContentType = PlainText

  def format(e: JposEntry): String = {
    e.exportAsSeq.collect { case (k, v) if !headerAttributes.contains(k) =>
      val translatedValue = dictionary.typeOf(k, e) match {
        case FieldType.GZippedString => GZip.unzip(v)
        case _ => dictionary.translateValue(k, e, v).map(t => s"$v ($t)").getOrElse(cleanJson(v))
      }
      nameType.flatMap(dictionary.nameOf(_, k, e))
        .map(name => s"  $k: $translatedValue [$name]")
        .getOrElse(s"  $k: $translatedValue")
    }.mkString(entryHeading(e), "\n", "\n</log>\n")
  }

  private def entryHeading(e: JposEntry) = {
    val b = new StringBuilder("<log")
    addAttribute(b, "realm", e.realm.toString)
    addAttribute(b, "at", DefaultDateTimeFormat.print(e.timestamp))
    val lifespan = e.lifespan
    if (e.msgType != null) {
      addAttribute(b, "type", e.msgType)
    }
    if (lifespan.isDefined) {
      addAttribute(b, "lifespan", lifespan.get.toString)
    }
    b.append(">\n")
    b.toString()
  }

  private def addAttribute(b: StringBuilder, name: String, value: String) = {
    b.append(' ').append(name).append("=\"").append(value).append('"')
  }

  private def cleanJson(s: String) = s.replace("&quot;", "\"")

  private val headerAttributes = Set("realm", "at", "msgType", "lifespan")
}
