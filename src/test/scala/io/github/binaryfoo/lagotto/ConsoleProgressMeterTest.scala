package io.github.binaryfoo.lagotto

import java.io.{PrintStream, ByteArrayOutputStream}

import org.joda.time.DateTimeUtils
import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class ConsoleProgressMeterTest extends LagoTest with BeforeAndAfter {

  "Progress" should "be written" in {
    val (meter, out) = newMeter()
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

    sane(out) shouldEqual "\n" +
                          "\\rOn number-1.log 1 of 10 (0 logs/ms) T+ 0s T- 0s\n" +
                          "\\rOn number-2.log 2 of 10 (10 logs/ms) T+ 1s T- 8s\n" +
                          "\\rOn 3.log 3 of 10 (1 logs/ms) T+ 1s T-           \n" +
                          "\\rTook 1 minute 1s for 10002 logs (0:0:10/ms)\n"
  }

  it should "write progress within a file" in {
    val (meter, out) = newMeter()
    val start = System.currentTimeMillis()
    DateTimeUtils.setCurrentMillisFixed(start)

    meter.startRun(2)
    meter.startFile("number-1.log")
    DateTimeUtils.setCurrentMillisFixed(start + 1000)
    meter.progressInFile(10000)

    sane(out) shouldEqual "\n" +
      "\\rOn number-1.log 1 of 2 (0 logs/ms) T+ 0s T- 0s\n" +
      "\\rOn number-1.log 1 of 2 (10 logs/ms) T+ 1s T- 0s"
  }

  private def newMeter() = {
    val out = new ByteArrayOutputStream()
    val meter = new ConsoleProgressMeter(new PrintStream(out))
    (meter, out)
  }

  private def sane(out: ByteArrayOutputStream): String = out.toString.replace("\r", "\n\\r")

  after {
    DateTimeUtils.setCurrentMillisSystem()
  }

}
