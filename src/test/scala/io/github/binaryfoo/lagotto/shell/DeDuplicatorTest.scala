package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.{JposEntry, LagoTest}

class DeDuplicatorTest extends LagoTest {

  "Duplicate" should "be filtered" in {
    val in = List(JposEntry(lines = "line 1"), JposEntry(lines = "line 2"), JposEntry(lines = "line 1"))
    in.filter(new DeDuplicator) shouldBe List(JposEntry(lines = "line 1"), JposEntry(lines = "line 2"))
  }
}
