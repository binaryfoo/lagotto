package io.github.binaryfoo.isotools

trait ConvertibleToMap {

  def apply(id: String): String

  def toMap(ids: String*): Map[String, String] = toMap(List(ids : _*))

  def toMap(ids: Iterable[String]): Map[String, String] = ids.map(id => id -> apply(id)).toMap
}
