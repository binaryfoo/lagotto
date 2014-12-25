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

    val sanitized = out.toString.replace("\r", "\n\\r")
    sanitized shouldEqual "\n" +
                          "\\rOn number-1.log 1 of 10 (0 logs/ms) T+ 0s T- 0s\n" +
                          "\\rOn number-2.log 2 of 10 (10 logs/ms) T+ 1s T- 8s\n" +
                          "\\rOn 3.log 3 of 10 (1 logs/ms) T+ 1s T-           \n" +
                          "\\rTook 1 minute 1s for 10002 logs (0:0:10/ms)\n"
  }
}
