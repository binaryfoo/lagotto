package io.github.binaryfoo.isotools

trait ConvertibleToMap {

  def apply(id: String): String

  def toMap(ids: String*): Map[String, String] = toMap(ids.toIterable)

  def toMap(ids: Iterable[String]): Map[String, String] = ids.map(id => id -> apply(id)).toMap

  def toCsv(ids: String*): String = toCsv(ids.toIterable)

  def toCsv(ids: Iterable[String]): String = Csv.toCsv(toMap(ids))

}

object ConvertibleToMap {

  implicit class IterableOfConvertibleToMap(val v: Iterable[ConvertibleToMap]) extends AnyVal {

    def toCsv(ids: String*): String = toCsv(ids.toIterable)

    def toCsv(ids: Iterable[String]): String = v.map(m => Csv.toCsv(m.toMap(ids))).mkString("\n")

  }
}
