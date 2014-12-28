package io.github.binaryfoo.lagotto

import org.scalatest.{Matchers, FlatSpec}

class LogFieldExprTest extends FlatSpec with Matchers {

  "Subtract expression" should "diff two times" in {
    val expr = LogFieldExpr.unapply("calc(max(time)-min(time))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(time)" -> "03:03:03.333", "min(time)" -> "02:02:02.222")))
    diff shouldBe "01:01:01.111"
    expr.toString() shouldBe "calc(max(time)-min(time))"
  }

  it should "always show zero for times" in {
    val expr = LogFieldExpr.unapply("calc(max(time)-min(time))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(time)" -> "02:02:02.222", "min(time)" -> "02:02:02.222")))
    diff shouldBe "00:00:00.000"
  }

  it should "handle custom time format HH:m0" in {
    val expr = LogFieldExpr.unapply("calc(max(time(HH:m0))-min(time(HH:m0)))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(time(HH:m0))" -> "03:30", "min(time(HH:m0))" -> "02:20")))
    diff shouldBe "01:10"
  }

  it should "handle full date with difference of days" in {
    val expr = LogFieldExpr.unapply("calc(max(date)-min(date))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(date)" -> "2015-01-01", "min(date)" -> "2014-12-31")))
    diff shouldBe "1 day"
  }

  it should "handle full date with difference of months" in {
    val expr = LogFieldExpr.unapply("calc(max(date)-min(date))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(date)" -> "2015-01-01", "min(date)" -> "2014-11-30")))
    diff shouldBe "1 month 2 days"
  }

  it should "handle full date with difference of years" in {
    val expr = LogFieldExpr.unapply("calc(max(date)-min(date))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(date)" -> "2017-01-01", "min(date)" -> "2014-10-30")))
    diff shouldBe "2 years 2 months 2 days"
  }

  it should "handle full timestamp with difference of days" in {
    val expr = LogFieldExpr.unapply("calc(max(timestamp)-min(timestamp))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(timestamp)" -> "2015-01-01 17:03:33.333", "min(timestamp)" -> "2014-12-31 16:02:22.222")))
    diff shouldBe "1 day 01:01:11.111"
  }

  it should "handle full timestamp with difference of months" in {
    val expr = LogFieldExpr.unapply("calc(max(timestamp)-min(timestamp))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(timestamp)" -> "2015-01-01 17:03:33.333", "min(timestamp)" -> "2014-11-30 16:02:22.222")))
    diff shouldBe "1 month 2 days 01:01:11.111"
  }

  it should "handle full timestamp with difference of years" in {
    val expr = LogFieldExpr.unapply("calc(max(timestamp)-min(timestamp))").get
    val diff = expr(AggregateLogLike(Map(), Seq("max(timestamp)" -> "2015-01-01 17:03:33.333", "min(timestamp)" -> "2013-10-30 16:02:22.222")))
    diff shouldBe "1 year 2 months 2 days 01:01:11.111"
  }
}