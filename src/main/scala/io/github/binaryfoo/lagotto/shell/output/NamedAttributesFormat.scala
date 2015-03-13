package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.dictionary.DataDictionary
import io.github.binaryfoo.lagotto.shell.{ContentType, PlainText, OutputFormat}
import io.github.binaryfoo.lagotto.{JposEntry, LogEntry}

import scala.collection.mutable

case class NamedAttributesFormat(dictionary: DataDictionary) extends OutputFormat {

  override def header(): Option[String] = None
  override def footer(): Option[String] = None
  override def apply(e: LogEntry): Option[String] = {
    e match {
      case j: JposEntry => Some(addAttributeNames(e))
      case _ => Some(e.lines)
    }
  }
  override val contentType: ContentType = PlainText

  private def addAttributeNames(e: LogEntry): String = {
    val paths = mutable.HashMap[String, String]()

    def recorder(path: String, line: String): Unit = paths.put(line, path)

    val lines = e.lines.split('\n').toSeq
    JposEntry.extractFields(lines, recorder)
    lines.map { l =>
      paths.get(l)
        .flatMap(path => dictionary.englishNameOf(path, e).map((path, _)))
        .map { case (path, name) =>
        val id = lastElement(paths(l))
        val idAttribute = s"""id="$id""""
        val nameAttribute = s""" name="$name""""
        l.replace(idAttribute, idAttribute + nameAttribute)
      }.getOrElse(l)
    }.mkString("\n")
  }

  private def lastElement(path: String) = {
    val lastDot = path.lastIndexOf('.')
    if (lastDot == -1) path
    else path.substring(lastDot + 1)
  }
}
