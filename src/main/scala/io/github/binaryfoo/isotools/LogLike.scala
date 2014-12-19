package io.github.binaryfoo.isotools

import org.joda.time.DateTime

import scala.collection.immutable.ListMap

trait LogLike {

  def timestamp: DateTime

  def apply(id: String): String

  def lines: String

  def toMap(ids: String*): Map[String, String] = toMap(ids.toIterable)

  def toMap(ids: Iterable[String]): Map[String, String] = {
    val pairs = ids.map { id =>
      val value = apply(id)
      id -> (if (value == null) "" else value)
    }
    ListMap[String, String](pairs.toSeq: _*)
  }

  def toCsv(ids: String*): String = toCsv(ids.toIterable)

  def toCsv(ids: Iterable[String]): String = Xsv.toCsv(toMap(ids))

  def toTsv(ids: String*): String = toTsv(ids.toIterable)

  def toTsv(ids: Iterable[String]): String = Xsv.toTsv(toMap(ids))

}

object LogLike {

  implicit class IterableOfLogLike(val v: Iterable[LogLike]) extends AnyVal {

    def toCsv(ids: String*): String = toCsv(ids.toIterable)

    def toCsv(ids: Iterable[String]): String = v.map(_.toCsv(ids)).mkString("\n")

    def toTsv(ids: String*): String = toTsv(ids.toIterable)

    def toTsv(ids: Iterable[String]): String = v.map(_.toTsv(ids)).mkString("\n")

  }
}
