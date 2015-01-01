package io.github.binaryfoo.lagotto.output

import org.scalatest.{FlatSpec, Matchers}

class AsciiTableTest extends FlatSpec with Matchers {

  "Convenience from" should "build a table" in {
    val table = AsciiTable.from(Seq("one", "two"), Seq(Seq("r1a", "r1b"), Seq("r2a", "r2b"))).toString
    table shouldBe """=============
                     || one | two |
                     |=============
                     || r1a | r1b |
                     || r2a | r2b |
                     |=============
                     |""".stripMargin
  }

}
