package io.github.binaryfoo.lagotto

import org.scalatest.{Matchers, FlatSpec}
import io.github.binaryfoo.lagotto.OrderedGroupBy.Implicit

class OrderedGroupByImplicitTest extends FlatSpec with Matchers {

  "Ordered groupBy" should "retain order in which keys are encountered" in {
    val list: List[(String, String)] = List("2" -> "2a", "1" -> "1a", "2" -> "2b")
    val grouped: Map[String, List[(String, String)]] = list.groupByOrdered(_._1)
    grouped.toList shouldEqual List(("2", List("2" -> "2a", "2" -> "2b")), ("1", List("1" -> "1a")))
  }
}
