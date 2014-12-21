package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

import scala.language.implicitConversions

trait LogLike {

  def timestamp: DateTime

  def apply(id: String): String

  def lines: String

  def toSeq(ids: String*): Seq[String] = toSeq(ids.toIterable)

  def toSeq(ids: Iterable[String]): Seq[String] = {
    ids.map { id =>
      val value = apply(id)
      if (value == null) "" else value
    }.toSeq
  }

  def exprToSeq(ids: Iterable[LogFieldExpr]): Seq[String] = {
    ids.map { id =>
      val value = id(this)
      if (value == null) "" else value
    }.toSeq
  }

  def toCsv(ids: String*): String = toCsv(ids.toIterable)

  def toCsv(ids: Iterable[String]): String = Xsv.toCsv(toSeq(ids))

  def toTsv(ids: String*): String = toTsv(ids.toIterable)

  def toTsv(ids: Iterable[String]): String = Xsv.toTsv(toSeq(ids))

  def toXsv(separator: String, ids: LogFieldExpr*): String = toXsv(separator, ids.toIterable)

  def toXsv(separator: String, ids: Iterable[LogFieldExpr]): String = Xsv.toXsv(separator, exprToSeq(ids))

}

object LogLike {

  implicit class IterableOfLogLike(val v: Iterable[LogLike]) extends AnyVal {

    def toCsv(ids: String*): String = toCsv(ids.toIterable)

    def toCsv(ids: Iterable[String]): String = v.map(_.toCsv(ids)).mkString("\n")

    def toTsv(ids: String*): String = toTsv(ids.toIterable)

    def toTsv(ids: Iterable[String]): String = v.map(_.toTsv(ids)).mkString("\n")

  }

  implicit def stringAsLogFieldExpr(s: String): LogFieldExpr = { e: LogLike => e(s) }
}
