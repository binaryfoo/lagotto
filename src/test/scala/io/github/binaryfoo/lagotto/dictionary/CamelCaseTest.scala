package io.github.binaryfoo.lagotto.dictionary

import io.github.binaryfoo.lagotto.LagoTest

class CamelCaseTest extends LagoTest {

  "toCamelCase()" should "lowercase the first letter" in {
    CamelCase.toCamelCase("Up") shouldBe "up"
  }

  it should "uppercase each letter after a space and remove the space" in {
    CamelCase.toCamelCase("lower me up Town") shouldBe "lowerMeUpTown"
  }

  it should "strip no anything other than [a-zA-Z0-9_]" in {
    // not quite comprehensive...
    CamelCase.toCamelCase("a - dash") shouldBe "aDash"
  }

  it should "translate a leading number to a word" in {
    CamelCase.toCamelCase("1 digit") shouldBe "oneDigit"
  }

  it should "leave other digits alone" in {
    CamelCase.toCamelCase("Digit #0") shouldBe "digit0"
    CamelCase.toCamelCase("Digit #0 end") shouldBe "digit0End"
  }

  "upperUnderscoredToSentence" should "decapitalise all but the first letter" in {
    SentenceCase.toSentence("ONE") shouldBe "One"
  }

  it should "replace '_' with ' '" in {
    SentenceCase.toSentence("ONE_TWO") shouldBe "One Two"
  }
}
