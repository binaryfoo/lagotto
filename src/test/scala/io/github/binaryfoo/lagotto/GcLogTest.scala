package io.github.binaryfoo.lagotto

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.reader.LogTypes
import org.joda.time.{DateTimeZone, DateTime}

class GcLogTest extends LagoTest {

  private val logTypes = LogTypes.load(ConfigFactory.load())
  private val gcLogType = logTypes.apply("gc")

  "Gc log" should "parse full GC" in {
    val fullGcLine = """2015-12-24T13:55:42.179+1100: 1727104.239: [Full GC [PSYoungGen: 1221184K->800989K(1293440K)] [ParOldGen: 2796219K->2796219K(2796224K)] 4017403K->3597213K(4089664K) [PSPermGen: 67955K->67955K(68224K)], 3.2324890 secs] [Times: user=11.20 sys=0.01, real=3.24 secs]"""
    val entry = gcLogType.apply(lineIteratorFrom(fullGcLine))

    entry.timestamp shouldBe new DateTime(2015, 12, 24, 13, 55, 42, 179, DateTimeZone.forOffsetHours(11))
    entry("pause") shouldBe "3.232489"
    entry("heapBefore") shouldBe "4113820672"
    entry("heapAfter") shouldBe "3683546112"
    entry("heapMax") shouldBe "4187815936"
    entry("type") shouldBe "Full GC"

    entry("PSYoungGenBefore") shouldBe "1250492416"
    entry("PSYoungGenAfter") shouldBe "820212736"
    entry("PSYoungGenMax") shouldBe "1324482560"

    entry("ParOldGenBefore") shouldBe "2863328256"
    entry("ParOldGenAfter") shouldBe "2863328256"
    entry("ParOldGenMax") shouldBe "2863333376"
  }

  it should "parse failed promotion" in {
    val line = """2015-12-24T13:55:39.305+1100: 1727101.365: [GC-- [PSYoungGen: 1221184K->1221184K(1293440K)] 4017395K->4017403K(4089664K), 2.8646910 secs] [Times: user=3.82 sys=3.12, real=2.87 secs]"""
    val entry = gcLogType.apply(lineIteratorFrom(line))

    entry.timestamp shouldBe new DateTime(2015, 12, 24, 13, 55, 39, 305, DateTimeZone.forOffsetHours(11))
    entry("pause") shouldBe "2.864691"
    entry("heapBefore") shouldBe "4113812480"
    entry("heapAfter") shouldBe "4113820672"
    entry("heapMax") shouldBe "4187815936"
    entry("type") shouldBe "GC--"

    entry("PSYoungGenBefore") shouldBe "1250492416"
    entry("PSYoungGenAfter") shouldBe "1250492416"
    entry("PSYoungGenMax") shouldBe "1324482560"
  }

  it should "parse total times" in {
    val lines = lineIteratorFrom(contentsOf("gc-with-total-times.txt"))
    val entry = gcLogType.apply(lines)
    println(entry)
  }
}
