package io.github.binaryfoo.lagotto

object Xsv {

  def toCsv(m: Map[String, String]): String = m.values.mkString(",")

  def toTsv(m: Map[String, String]): String = m.values.mkString("\t")

  implicit class MapToXsv(val m: Map[String, String]) extends AnyVal {
    def toCsv: String = Xsv.toCsv(m)
    def toTsv: String = Xsv.toTsv(m)
  }

  implicit class IterableOfMapToXsv(val v: Iterable[Map[String,String]]) extends AnyVal {
    def toCsv: String = v.map(_.toCsv).mkString("\n")
    def toTsv: String = v.map(_.toTsv).mkString("\n")
  }

}
