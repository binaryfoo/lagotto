package io.github.binaryfoo.lagotto

import java.io.{PrintStream, ByteArrayOutputStream}

import org.joda.time.DateTimeUtils
import org.scalatest.{Matchers, FlatSpec}

class ConsoleProgressMeterTest extends FlatSpec with Matchers {

  "Progress" should "be written" in {
    val out = new ByteArrayOutputStream()
    val meter = new ConsoleProgressMeter(new PrintStream(out))

    val start = System.currentTimeMillis()
    DateTimeUtils.setCurrentMillisFixed(start)
    meter.startRun(10)
    meter.startFile("number-1.log")
    DateTimeUtils.setCurrentMillisFixed(start + 1000)
    meter.finishFile(10000)
    meter.startFile("number-2.log")
    DateTimeUtils.setCurrentMillisFixed(start + 1000)
    meter.finishFile(1)
    meter.startFile("3.log")
    DateTimeUtils.setCurrentMillisFixed(start + 61001)
    meter.finishFile(1)
    meter.finish()

    out.toString.replace("\r", "\n\\r") shouldEqual "\n" +
                                                    "\\rOn number-1.log 1 of 10 0 entries (0/ms) T 0s E 0s\n" +
                                                    "\\rOn number-2.log 2 of 10 10000 entries (10/ms) T 1s E 8s\n" +
                                                    "\\rOn 3.log 3 of 10 10001 entries (1/ms) T 1s E 0.007s    \n" +
                                                    "\\rTook 1 minute 1.001s 10002 (0/ms)                  \n"
  }
}
