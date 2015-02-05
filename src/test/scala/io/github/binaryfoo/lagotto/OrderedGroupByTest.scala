package io.github.binaryfoo.lagotto

import org.scalatest.{Matchers, FlatSpec}

import scala.collection.{immutable, mutable}

class OrderedGroupByTest extends FlatSpec with Matchers {

  "Ordered groupBy" should "retain order in which keys are encountered" in {
    val list: List[(String, String)] = List("2" -> "2a", "1" -> "1a", "2" -> "2b")
    val key: ((String, String)) => String = { case (k, v) => k }
    def newBuilder(k: String): mutable.Builder[(String, String), List[(String, String)]] = immutable.List.newBuilder
    val grouped = OrderedGroupBy.groupByOrdered(list.toIterator, key, newBuilder)
    grouped shouldEqual Seq(List("2" -> "2a", "2" -> "2b"), List("1" -> "1a"))
  }
}
