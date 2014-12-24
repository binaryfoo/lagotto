package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.{LogFilter, LogEntry}
import org.scalatest.{Matchers, FlatSpec}

class LogFilterTest extends FlatSpec with Matchers {

  "Contains operator" should "use a regex if value like /regex/" in {
    val filter = """mti~/\d\d[13]\d/""" match {
      case LogFilter(f) => f
    }
    filter(LogEntry("0" -> "0210")) shouldBe true
    filter(LogEntry("0" -> "0200")) shouldBe false
    filter(LogEntry("0" -> "0230")) shouldBe true
    filter(LogEntry("0" -> "0220")) shouldBe false
    filter(LogEntry("0" -> "023")) shouldBe false
  }

  it should "negate the regex for value like /regex/ with operator !~" in {
    val filter = """mti!~/\d\d[13]\d/""" match {
      case LogFilter(f) => f
    }
    filter(LogEntry("0" -> "0210")) shouldBe false
    filter(LogEntry("0" -> "0200")) shouldBe true
    filter(LogEntry("0" -> "0230")) shouldBe false
    filter(LogEntry("0" -> "0220")) shouldBe true
    filter(LogEntry("0" -> "023")) shouldBe true
  }

  "Regex contains operator" should "not match null" in {
    val filter = """mti!~/3.*/""" match {
      case LogFilter(f) => f
    }
    filter(LogEntry()) shouldBe false
  }
}
