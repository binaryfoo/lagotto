package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.mutable

case class JstackLogEntry(private val _fields: mutable.LinkedHashMap[String, String], private val timeFormat: Option[TimeExpr] = None, lines: String, source: SourceRef = null) extends LogEntry {

  val timestamp: DateTime = {
    timeFormat.map { case TimeExpr(expr, formatter) =>
      _fields.get(expr)
        .map(formatter.parseDateTime)
        .getOrElse(throw new IAmSorryDave(s"Missing 'timestamp' in ${_fields}"))
    }.orNull
  }

  private val Frame = """frame\((\d+)\)""".r

  val fields = _fields.withDefault {
    case "depth" => frames.length.toString
    case Frame(Index(index)) if index < frames.length => frames(index).replaceFirst("\\s*at ", "")
    case _ => null
  }

  lazy val frames: Array[String] = {
    lines.split('\n').filter(_.trim.startsWith("at "))
  }

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

private object Index {
  def unapply(s: String): Option[Int] = Some(s.toInt)
}