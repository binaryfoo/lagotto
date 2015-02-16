package io.github.binaryfoo.lagotto.shell

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.LagoTest

class PipelineTest extends LagoTest {

  import io.github.binaryfoo.lagotto.LogFilters.NaiveParser.LogFilter.filterFor

  "Filter partitioning" should "apply count(exception!=)>3 after aggregation" in {
    val moreThanOneException = filterFor("count(exception!=)>1)")
    val opts = CmdLineOptions(null, filters = Seq(moreThanOneException))
    val Filters(aggregate, Nil, Nil) = new Pipeline(opts, ConfigFactory.load()).partitionFilters()

    aggregate shouldBe Seq(moreThanOneException)
  }

  it should "apply delay>1 after delay but before aggregation" in {
    val slow = filterFor("delay>1")
    val opts = CmdLineOptions(null, filters = Seq(slow))
    val Filters(Nil, delay, Nil) = new Pipeline(opts, ConfigFactory.load()).partitionFilters()

    delay shouldBe Seq(slow)
  }
}
