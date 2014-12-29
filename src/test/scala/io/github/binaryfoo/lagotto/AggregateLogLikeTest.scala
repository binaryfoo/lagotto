package io.github.binaryfoo.lagotto

import org.scalatest.{Matchers, FlatSpec}

class AggregateLogLikeTest extends FlatSpec with Matchers {

  private val threeStans = Stream(
    LogEntry("0" -> "0200", "11" -> "1"),
    LogEntry("0" -> "0200", "11" -> "2"),
    LogEntry("0" -> "0210", "11" -> "3"))

  "Aggregation" should "support count" in {
    aggregateToCsv(threeStans, "mti", "count") shouldBe List("0200,2", "0210,1")
  }

  it should "support count(distinct(field))" in {
    aggregateToCsv(threeStans, "count(distinct(mti))") shouldBe List("2")
  }

  private val twoLifespans = Stream(LogEntry("lifespan" -> "100"), LogEntry("lifespan" -> "200"))

  it should "support avg(field)" in {
    aggregate("avg(lifespan)") shouldBe List("150")
    aggregateToCsv(threeStans, "mti", "avg(11)") shouldBe List("0200,1", "0210,3")
  }

  it should "support min(field)" in {
    aggregate("min(lifespan)") shouldBe List("100")
    aggregateToCsv(threeStans, "mti", "min(11)") shouldBe List("0200,1", "0210,3")
  }

  it should "support max(field)" in {
    aggregate("max(lifespan)") shouldBe List("200")
    aggregateToCsv(threeStans, "mti", "max(11)") shouldBe List("0200,2", "0210,3")
  }

  it should "support sum(field)" in {
    aggregate("sum(lifespan)") shouldBe List("300")
    aggregateToCsv(threeStans, "mti", "sum(11)") shouldBe List("0200,3", "0210,3")
  }

  it should "support group_concat(field)" in {
    aggregate("group_concat(lifespan)") shouldBe List("100,200")
    aggregateToCsv(threeStans, "mti", "group_concat(11)") shouldBe List("0200,1,2", "0210,3")
  }

  private val twoStrings = Stream(
    LogEntry("48" -> "a"),
    LogEntry("48" -> "b"))


  "min(field)" should "support string comparison" in {
    aggregateToCsv(twoStrings, "min(48)") shouldBe List("a")
  }

  "max(field)" should "support string comparison" in {
    aggregateToCsv(twoStrings, "max(48)") shouldBe List("b")
  }

  private def aggregate(field: String): List[String] = {
    val aggregationConfig = AggregationSpec.fromExpressions(LogFieldExpr.expressionsFor(field))
    val aggregated = AggregateLogLike.aggregate(twoLifespans.toIterator, aggregationConfig.keys, aggregationConfig.aggregates.toSeq)
    aggregated.map(_(field)).toList
  }

  private def aggregateToCsv(raw: Stream[LogLike], fields: String*): List[String] = {
    val aggregationConfig = AggregationSpec.fromExpressions(LogFieldExpr.expressionsFor(fields))
    val aggregated = AggregateLogLike.aggregate(raw.toIterator, aggregationConfig.keys, aggregationConfig.aggregates.toSeq)
    aggregated.map(_.toCsv(fields)).toList
  }
}
