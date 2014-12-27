package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.LogFilter.filterFor
import org.scalatest.{FlatSpec, Matchers}

class PipelineTest extends FlatSpec with Matchers {

  "Filter partitioning" should "apply count(exception!=)>3 after aggregation" in {
    val moreThanOneException = filterFor("count(exception!=)>1)")
    val config = Config(filters = Seq(moreThanOneException))
    val Filters(aggregate, Nil, Nil) = new Pipeline(config).partitionFilters()

    aggregate shouldBe Seq(moreThanOneException)
  }

  it should "apply delay>1 after delay but before aggregation" in {
    val slow = filterFor("delay>1")
    val config = Config(filters = Seq(slow))
    val Filters(Nil, delay, Nil) = new Pipeline(config).partitionFilters()

    delay shouldBe Seq(slow)
  }
}
