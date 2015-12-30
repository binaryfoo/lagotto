package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.reader.JvmGCLog
import org.joda.time.{DateTime, DateTimeZone}

class JvmGCLogTest extends LagoTest {

  "GC log" should "be parsed" in {
    val entry = JvmGCLog.apply(lineIteratorFrom(contentsOf("gc.log")))
    entry.timestamp shouldBe new DateTime(2015, 1, 8, 10, 18, 13, 300, DateTimeZone.forOffsetHours(11))
    entry("heapBefore") shouldBe "2871497728"
    entry("heapAfter") shouldBe "1489845248"
    entry("heapMax") shouldBe "4270260224"
  }
}
