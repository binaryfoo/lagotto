package io.github.binaryfoo.lagotto.output

import io.github.binaryfoo.lagotto.LagoTest

class GZipTest extends LagoTest {

  "Unzip" should "hunt for GZIP magic header" in {
    val twoByteHeader = "00001F8B0800000000000000CB48CDC9C95748C94C4F2D2E010037FA227F0C000000"
    GZip.unzip(twoByteHeader) shouldBe "hello digest"
  }

  it should "handle no leading padding" in {
    val noHeader = "1F8B0800000000000000CB48CDC9C95748C94C4F2D2E010037FA227F0C000000"
    GZip.unzip(noHeader) shouldBe "hello digest"
  }

  "Zip" should "gzip and hex encode" in {
    GZip.zip("hello digest") shouldBe "1F8B0800000000000000CB48CDC9C95748C94C4F2D2E010037FA227F0C000000"
  }
}
