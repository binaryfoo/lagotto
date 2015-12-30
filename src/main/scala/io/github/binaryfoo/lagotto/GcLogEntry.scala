package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.collection.mutable

/**
 * Ceremony around a Map.
 */
case class GcLogEntry(timestamp: DateTime,
                      private val _fields: mutable.LinkedHashMap[String, String],
                      lines: String,
                      source: SourceRef = null) extends LogEntry {

  val fields = _fields.withDefaultValue(null)

  def apply(id: String) = fields(id)

  override def exportAsSeq: Seq[(String, String)] = _fields.toSeq
}

