package io.github.binaryfoo.lagotto

object Xsv {

  def toCsv(m: Seq[String]): String = toXsv(",", m)

  def toTsv(m: Seq[String]): String = toXsv("\t", m)

  def toXsv(separator: String, m: Seq[String]): String = m.mkString(separator)

  implicit class MapToXsv(val m: Seq[String]) extends AnyVal {
    def toCsv: String = Xsv.toCsv(m)
    def toTsv: String = Xsv.toTsv(m)
  }

}
