package io.github.binaryfoo.lagotto.digest

import io.github.binaryfoo.lagotto.JposEntry

import scala.collection.mutable

case class HistogramSet(key: JposEntry => String, keyName: String, grams: Histogram*) {

  def add(e: JposEntry) = {
    grams.foreach(_.add(e, key))
  }

  def toXsv(separator: String = "\t"): List[String] = {
    val keys = grams.flatMap(_.counts.keySet).toList.sorted
    val rows = keys.map { t => (t :: grams.map(_.counts.getOrElse(t, 0)).toList).mkString(separator)}
    val header = (keyName :: grams.map(_.name).toList).mkString(separator)
    header :: rows
  }
}

case class Histogram(name: String, filter: JposEntry => Boolean) {
  val counts: mutable.Map[String, Int] = mutable.Map()

  def add(e: JposEntry, key: JposEntry => String): Unit = {
    if (filter(e)) {
      val k = key(e)
      counts.update(k, counts.getOrElse(k, 0) + 1)
    }
  }
}