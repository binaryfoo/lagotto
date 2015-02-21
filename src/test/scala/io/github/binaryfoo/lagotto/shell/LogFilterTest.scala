package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.{AggregateLogEntry, JposEntry, LagoTest}

class LogFilterTest extends LagoTest {

  import io.github.binaryfoo.lagotto.LogFilters.NaiveParser.LogFilter
  import io.github.binaryfoo.lagotto.LogFilters.NaiveParser.LogFilter.filterFor
  import io.github.binaryfoo.lagotto.LogFilters.NaiveParser.parseAndExpr

  "Contains operator" should "use a regex if value like /regex/" in {
    val filter = filterFor("""mti~/\d\d[13]\d/""")
    filter(JposEntry("0" -> "0210")) shouldBe true
    filter(JposEntry("0" -> "0200")) shouldBe false
    filter(JposEntry("0" -> "0230")) shouldBe true
    filter(JposEntry("0" -> "0220")) shouldBe false
    filter(JposEntry("0" -> "023")) shouldBe false
  }

  it should "negate the regex for value like /regex/ with operator !~" in {
    val filter = filterFor("""mti!~/\d\d[13]\d/""")
    filter.field shouldBe "mti"
    filter(JposEntry("0" -> "0210")) shouldBe false
    filter(JposEntry("0" -> "0200")) shouldBe true
    filter(JposEntry("0" -> "0230")) shouldBe false
    filter(JposEntry("0" -> "0220")) shouldBe true
    filter(JposEntry("0" -> "023")) shouldBe true
  }

  "in operator" should "match value in set" in {
    val filter = filterFor("socket in (10.0.0.1:8000,10.0.0.2:8001)")
    filter.field shouldBe "socket"
    filter(JposEntry("realm" -> "channel/10.0.0.1:8000")) shouldBe true
    filter(JposEntry("realm" -> "channel/10.0.0.1:8001")) shouldBe false
    filter(JposEntry("realm" -> "channel/10.0.0.2:8001")) shouldBe true
    filter(JposEntry("realm" -> "channel/10.0.0.2:8000")) shouldBe false
    filter(JposEntry("realm" -> "channel/10.0.0.3:8000")) shouldBe false
  }

  "not operator" should "match value not in set" in {
    val filter = filterFor("msgType not in (send,receive)")
    filter.field shouldBe "msgType"
    filter(JposEntry("msgType" -> "bollox")) shouldBe true
    filter(JposEntry("msgType" -> "send")) shouldBe false
    filter(JposEntry("msgType" -> "receive")) shouldBe false
  }

  "Equals operator" should "allow comparison with empty string" in {
    val filter = filterFor("exception!=")
    filter.field shouldBe "exception"
    filter(JposEntry("exception" -> "oh my")) shouldBe true
    filter(JposEntry("exception" -> "")) shouldBe false
    filter(JposEntry()) shouldBe false
  }

  "Regex contains operator" should "not match null" in {
    val filter = filterFor("""mti!~/3.*/""")
    filter(JposEntry()) shouldBe false
  }

  "Greater than operator" should "be applicable to the result of count(condition)" in {
    val filter = filterFor("count(mti=0200)>10")
    filter.field shouldBe "count(mti=0200)"
    filter(AggregateLogEntry(Map.empty, Seq("count(mti=0200)" -> "10"))) shouldBe true
    filter(AggregateLogEntry(Map.empty, Seq("count(mti=0200)" -> "9"))) shouldBe false
  }

  "Regex operator" should "be applicable to the result of count(condition)" in {
    val filter = filterFor("count(mti=0200)~/1[01]1/")
    filter.field shouldBe "count(mti=0200)"
    filter(AggregateLogEntry(Map.empty, Seq("count(mti=0200)" -> "101"))) shouldBe true
    filter(AggregateLogEntry(Map.empty, Seq("count(mti=0200)" -> "111"))) shouldBe true
    filter(AggregateLogEntry(Map.empty, Seq("count(mti=0200)" -> "191"))) shouldBe false
  }

  "Filters" should "compare equal for the same expression" in {
    filterFor("lifespan=10") shouldEqual filterFor("lifespan=10")
    filterFor("lifespan>10") shouldEqual filterFor("lifespan>10")
    filterFor("lifespan<10") shouldEqual filterFor("lifespan<10")
    filterFor("lifespan~10") shouldEqual filterFor("lifespan~10")
  }

  "Filters" should "compare not equal for different expressions" in {
    filterFor("lifespan=10") shouldNot equal(filterFor("lifespan!=10"))
    filterFor("lifespan=10") shouldNot equal(filterFor("lifespan=11"))
    filterFor("lifespan=10") shouldNot equal(filterFor("delay=10"))
  }

  "AndFilter" should "pass when all children pass" in {
    val filter = parseAndExpr("0=0200,realm=scheme").get
    filter(JposEntry("0" -> "0200", "realm" -> "scheme")) shouldBe true
  }

  it should "pass fail when a children fails" in {
    val filter = parseAndExpr("0=0200,realm=scheme").get
    filter(JposEntry("0" -> "0210", "realm" -> "scheme")) shouldBe false
    filter(JposEntry("0" -> "0200", "realm" -> "silly")) shouldBe false

  }

  it should "work with only one child" in {
    val filter = parseAndExpr("0=0210").get
    filter(JposEntry("0" -> "0200")) shouldBe false
    filter(JposEntry("0" -> "0210")) shouldBe true
  }

  "ChannelWith" should "match immediate condition" in {
    val filter = LogFilter.unapply("channelWith(48=magic)").get
    filter(JposEntry("48" -> "magic")) shouldBe true
  }

  it should "match message with the same realm as a previously matched message" in {
    val filter = LogFilter.unapply("channelWith(48=magic)").get
    filter(JposEntry("48" -> "magic", "realm" -> "some/0.0.0.0:1234")) // stateful filter
    filter(JposEntry("realm" -> "some/0.0.0.0:1234")) shouldBe true
    filter(JposEntry("realm" -> "some/0.0.0.0:1235")) shouldBe false
  }

  it should "not match message with the same realm as a previously matched message once that channel is closed" in {
    val filter = LogFilter.unapply("channelWith(48=magic)").get
    filter(JposEntry("48" -> "magic", "realm" -> "some/0.0.0.0:1234")) // stateful filter
    filter(JposEntry("realm" -> "some/0.0.0.0:1234", "msgType" -> "session-end")) shouldBe true
    filter(JposEntry("realm" -> "some/0.0.0.0:1234")) shouldBe false
  }
}
