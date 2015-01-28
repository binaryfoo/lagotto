package io.github.binaryfoo.lagotto.dictionary

import io.github.binaryfoo.lagotto.LagoTest

class FieldPathTest extends LagoTest {

  "range expression" should "expand" in {
    FieldPath.expand("1.{3..4}") shouldBe Seq("1.3", "1.4")
    FieldPath.expand("1.{3..4}.1") shouldBe Seq("1.3.1", "1.4.1")
  }

  it should "expand a beginning" in {
    FieldPath.expand("{3..4}") shouldBe Seq("3", "4")
  }

  "multiple expressions in a path" should "all be expanded" in {
    FieldPath.expand("1.{3..4}.0.{5..6}") shouldBe Seq("1.3.0.5", "1.4.0.5", "1.3.0.6", "1.4.0.6")
  }

  "regular expression" should "be itself" in {
    FieldPath.expand("1") shouldBe Seq("1")
  }

  it should "allow multiple parts" in {
    FieldPath.expand("1.2") shouldBe Seq("1.2")
  }
}
