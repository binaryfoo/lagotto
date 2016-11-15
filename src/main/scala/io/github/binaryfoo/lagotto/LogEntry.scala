package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.output.Xsv
import org.joda.time.DateTime

import scala.language.implicitConversions

trait LogEntry {

  def timestamp: DateTime

  def source: SourceRef

  /**
   * Provides access to fields, attributes and extra derived values.
   *
   * Examples: fields like 11 and 48.1.2, attributes like 'at' and 'realm', derived values like time, date and socket.
   * @param id A dot delimited ISO 8583 field number, an attribute from the &lt;log&gt; record or a named value like time.
   * @return The value or null (rather than an Option). Rationale for not using an Option to avoid verbosity. Maybe flawed.
   */
  def apply(id: String): String

  def get(id: String): Option[String] = Option(apply(id))

  def contains(id: String): Boolean = apply(id) != null

  /**
   * The record as text (XML)
   * @return XML
   */
  def lines: String

  def toSeq(ids: String*): Seq[String] = toSeq(ids.toIterable)

  def toSeq(ids: Iterable[String]): Seq[String] = {
    ids.map { id =>
      val value = apply(id)
      if (value == null) "" else value
    }.toSeq
  }

  def exprsToSeq(ids: FieldExpr*): Seq[String] = exprToSeq(ids.toIterable)

  /**
   * The implicit in io.github.binaryfoo.lagotto.FieldExprParser#stringAsFieldAccessor(java.lang.String) might be useful.
   */
  def exprToSeq(ids: FieldAccessor[this.type]*): Seq[String] = exprToSeq(ids.toIterable)

  def exprToSeq(ids: Iterable[FieldAccessor[this.type]]): Seq[String] = {
    ids.map { id =>
      val value = id(this)
      if (value == null) "" else value
    }.toSeq
  }

  def toCsv(ids: String*): String = toCsv(ids.toIterable)

  def toCsv(ids: Iterable[String]): String = Xsv.toCsv(toSeq(ids))

  def toTsv(ids: String*): String = toTsv(ids.toIterable)

  def toTsv(ids: Iterable[String]): String = Xsv.toTsv(toSeq(ids))

  def toXsv[T <: LogEntry](separator: String, ids: FieldAccessor[this.type]*): String = Xsv.toXsv(separator, exprToSeq(ids))

  def toXsv(separator: String, ids: Iterable[String]): String = Xsv.toXsv(separator, toSeq(ids))

  def exportAsSeq: Seq[(String, String)]

  def exportAsSeq(ids: Iterable[FieldExpr]): Seq[(String, String)] = {
    ids.map { id =>
      val value = id(this)
      (id.toString(), if (value == null) "" else value)
    }.toSeq
  }
}

object LogEntry {

  val empty = new LogEntry {

    override def exportAsSeq: Seq[(String, String)] = Seq.empty
    override def lines: String = ""
    override def source: SourceRef = null
    override def timestamp: DateTime = null
    override def apply(id: String): String = null

  }

  implicit class IterableOfLogEntry(val v: Iterator[LogEntry]) extends AnyVal {

    def toCsv(ids: String*): String = toCsv(ids.toIterable)

    def toCsv(ids: Iterable[String]): String = v.map(_.toCsv(ids)).mkString("\n")

    def exprToCsv(ids: FieldExpr*): String = v.map(e => Xsv.toCsv(e.exprsToSeq(ids :_*))).mkString("\n")

    def toTsv(ids: String*): String = toTsv(ids.toIterable)

    def toTsv(ids: Iterable[String]): String = v.map(_.toTsv(ids)).mkString("\n")

  }
}
