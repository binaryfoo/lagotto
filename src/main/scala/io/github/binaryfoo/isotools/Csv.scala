package io.github.binaryfoo.isotools

object Csv {

  implicit class MapToCsv(val m: Map[String, String]) extends AnyVal {
    def toCsv: String = m.values.mkString(",")
  }

  implicit class IterableOfMapToCsv(val v: Iterable[Map[String,String]]) extends AnyVal {
    def toCsv: String = v.map(_.toCsv).mkString("\n")
  }

  implicit class IterableOfConvertibleToMap(val v: Iterable[ConvertibleToMap]) extends AnyVal {
    
    def toCsv(ids: String*): String = toCsv(ids.toIterable)

    def toCsv(ids: Iterable[String]): String = v.map(_.toMap(ids).toCsv).mkString("\n")
    
  }
}
