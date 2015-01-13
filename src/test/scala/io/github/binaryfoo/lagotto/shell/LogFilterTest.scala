package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.{AndFilter, AggregateLogEntry, JposEntry}
import io.github.binaryfoo.lagotto.LogFilter.filterFor
import org.scalatest.{Matchers, FlatSpec}

class LogFilterTest extends FlatSpec with Matchers {

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
    val filter = AndFilter.from("0=0200,realm=scheme")
    filter(JposEntry("0" -> "0200", "realm" -> "scheme")) shouldBe true
  }

  it should "pass fail when a children fails" in {
    val filter = AndFilter.from("0=0200,realm=scheme")
    filter(JposEntry("0" -> "0210", "realm" -> "scheme")) shouldBe false
    filter(JposEntry("0" -> "0200", "realm" -> "silly")) shouldBe false

  }

  it should "work with only one child" in {
    val filter = AndFilter.from("0=0210")
    filter(JposEntry("0" -> "0200")) shouldBe false
    filter(JposEntry("0" -> "0210")) shouldBe true
  }
}
