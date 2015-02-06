package io.github.binaryfoo.lagotto

class SIUnitsFormatterTest extends LagoTest {

  import io.github.binaryfoo.lagotto.SIUnitsFormatter._

  "Formatter" should "apply multiples over 1k" in {
    format(1000) shouldBe "1k"
    format(1100) shouldBe "1.1k"
    format(1060) shouldBe "1.1k"
    format(1000000) shouldBe "1M"
    format(1000000000) shouldBe "1G"
    format(1000000000000L) shouldBe "1T"
  }

}
