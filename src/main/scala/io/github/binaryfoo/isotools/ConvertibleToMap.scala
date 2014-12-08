package io.github.binaryfoo.isotools

import scala.collection.immutable.ListMap

trait ConvertibleToMap {

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

  def toCsv(ids: Iterable[String]): String = Csv.toCsv(toMap(ids))

}

object ConvertibleToMap {

  implicit class IterableOfConvertibleToMap(val v: Iterable[ConvertibleToMap]) extends AnyVal {

    def toCsv(ids: String*): String = toCsv(ids.toIterable)

    def toCsv(ids: Iterable[String]): String = v.map(m => Csv.toCsv(m.toMap(ids))).mkString("\n")

  }
}
