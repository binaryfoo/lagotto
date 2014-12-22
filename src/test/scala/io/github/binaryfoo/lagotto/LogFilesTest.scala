package io.github.binaryfoo.lagotto

import org.scalatest.{Matchers, FlatSpec}
import io.github.binaryfoo.lagotto.LogFiles._

class LogFilesTest extends FlatSpec with Matchers {

  "Log sequence number" should "use N in blah.N.log" in {
    sequenceNumber(file("/some/directory/name.42.log")) shouldBe 42
  }

  it should "default to zero when missing" in {
    sequenceNumber(file("/some/directory/name.log")) shouldBe 0
    sequenceNumber(file("/some/directory/name.rubbish.log")) shouldBe 0
  }
}
