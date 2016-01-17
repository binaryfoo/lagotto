package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.reader.JvmGCLog
import org.joda.time.{DateTime, DateTimeZone}

class JvmGCLogTest extends LagoTest {

  "GC log" should "be parsed" in {
    val entry = new JvmGCLog().apply(lineIteratorFrom(contentsOf("gc.log")))
    entry.timestamp shouldBe new DateTime(2015, 1, 8, 10, 18, 13, 300, DateTimeZone.forOffsetHours(11))
    entry("heapBefore") shouldBe "2871497728"
    entry("heapAfter") shouldBe "1489845248"
    entry("heapMax") shouldBe "4270260224"
  }

  it should "calculate rates" in {
    val iterator = lineIteratorFrom(contentsOf("gc.log"))
    val log = new JvmGCLog()

    val entry1 = log.apply(iterator)
    entry1("heapAllocated") shouldBe "2871497728"
    entry1("heapAllocationRate") shouldBe "12"

    val entry2 = log.apply(iterator)
    entry2("heapAllocated") shouldBe "1375664128"
    entry2("heapAllocationRate") shouldBe "76"
  }
}
