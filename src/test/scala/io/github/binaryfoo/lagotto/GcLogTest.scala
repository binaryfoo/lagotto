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
    entry("pause") shouldBe "3.24"
    entry("before") shouldBe "4017403"
    entry("after") shouldBe "3597213"
    entry("heap") shouldBe "4089664"
    entry("type") shouldBe "Full GC"

    entry("youngBefore") shouldBe "1221184"
    entry("youngAfter") shouldBe "800989"
    entry("young") shouldBe "1293440"

    entry("oldBefore") shouldBe "2796219"
    entry("oldAfter") shouldBe "2796219"
    entry("old") shouldBe "2796224"
  }

  it should "parse failed promotion" in {
    val line = """2015-12-24T13:55:39.305+1100: 1727101.365: [GC-- [PSYoungGen: 1221184K->1221184K(1293440K)] 4017395K->4017403K(4089664K), 2.8646910 secs] [Times: user=3.82 sys=3.12, real=2.87 secs]"""
    val entry = gcLogType.apply(lineIteratorFrom(line))

    entry.timestamp shouldBe new DateTime(2015, 12, 24, 13, 55, 39, 305, DateTimeZone.forOffsetHours(11))
    entry("pause") shouldBe "2.87"
    entry("before") shouldBe "4017395"
    entry("after") shouldBe "4017403"
    entry("heap") shouldBe "4089664"
    entry("type") shouldBe "GC--"

    entry("youngBefore") shouldBe "1221184"
    entry("youngAfter") shouldBe "1221184"
    entry("young") shouldBe "1293440"
  }
}
