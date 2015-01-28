package io.github.binaryfoo.lagotto.dictionary

import io.github.binaryfoo.lagotto.LagoTest

class SnakeCaseTest extends LagoTest {

  "toCamelCase()" should "lowercase the first letter" in {
    SnakeCase.toSnakeCase("Up") shouldBe "up"
  }

  it should "replace spaces with underscores" in {
    SnakeCase.toSnakeCase("lower me up Town") shouldBe "lower_me_up_town"
  }

  it should "strip no anything other than [a-zA-Z0-9_]" in {
    // not quite comprehensive...
    SnakeCase.toSnakeCase("a - dash") shouldBe "a_dash"
  }

  it should "translate a leading number to a word" in {
    SnakeCase.toSnakeCase("1 digit") shouldBe "one_digit"
  }

  it should "undo camel case" in {
    SnakeCase.toSnakeCase("downUp") shouldBe "down_up"
  }

  it should "leave other digits alone" in {
    SnakeCase.toSnakeCase("Digit #0") shouldBe "digit_0"
    SnakeCase.toSnakeCase("Digit #0 end") shouldBe "digit_0_end"
  }

  it should "decapitalize all the letters" in {
    SnakeCase.toSnakeCase("ONE") shouldBe "one"
    SnakeCase.toSnakeCase("ONE_TWO") shouldBe "one_two"
  }
}
