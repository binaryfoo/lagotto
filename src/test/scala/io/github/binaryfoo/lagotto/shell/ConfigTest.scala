package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.{LagoTest, FieldExprParser}
import org.scalatest.{FlatSpec, Matchers}

class ConfigTest extends LagoTest {

  import fieldParser.FieldExpr._
  import io.github.binaryfoo.lagotto.LogFilters.NaiveParser.LogFilter.filterFor

  "Aggregation config" should "split key and aggregate fields" in {
    val config = Config(format = Tabular(fields = expressionsFor("mti,count")))
    val aggregationConfig = config.aggregationConfig()
    aggregationConfig.keys shouldBe Seq(expressionFor("mti"))
    aggregationConfig.aggregates.toSeq shouldBe Seq(expressionFor("count"))
  }

  it should "include aggregates used in filters" in {
    val config = Config(format = Tabular(fields = expressionsFor("mti")), filters = Seq(filterFor("count>1")))
    val aggregationConfig = config.aggregationConfig()
    aggregationConfig.keys shouldBe Seq(expressionFor("mti"))
    aggregationConfig.aggregates.toSeq shouldBe Seq(expressionFor("count"))
  }

  it should "not duplicate aggregates when used in both the field list and a filter" in {
    shouldNotDuplicate("count")
    shouldNotDuplicate("count(lifespan>10)")
    shouldNotDuplicate("count(distinct(mti))")
    shouldNotDuplicate("min(lifespan)")
    shouldNotDuplicate("max(lifespan)")
    shouldNotDuplicate("sum(lifespan)")
    shouldNotDuplicate("avg(lifespan)")
    shouldNotDuplicate("group_concat(48)")
  }

  def shouldNotDuplicate(aggregate: String): Unit = {
    val config = Config(format = Tabular(fields = expressionsFor(s"mti,$aggregate")), filters = Seq(filterFor(s"$aggregate>1")))
    val aggregationConfig = config.aggregationConfig()
    aggregationConfig.keys shouldBe Seq(expressionFor("mti"))
    aggregationConfig.aggregates.toSeq shouldBe Seq(expressionFor(aggregate))
  }
}
