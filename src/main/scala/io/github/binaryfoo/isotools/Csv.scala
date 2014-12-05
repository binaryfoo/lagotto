package io.github.binaryfoo.isotools

object Csv {

  def toCsv(m: Map[String, String]): String = m.values.mkString(",")

  implicit class MapToCsv(val m: Map[String, String]) extends AnyVal {
    def toCsv: String = Csv.toCsv(m)
  }

  implicit class IterableOfMapToCsv(val v: Iterable[Map[String,String]]) extends AnyVal {
    def toCsv: String = v.map(_.toCsv).mkString("\n")
  }

}
