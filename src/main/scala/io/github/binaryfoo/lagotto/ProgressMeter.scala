package io.github.binaryfoo.lagotto

import java.io.PrintStream
import java.text.DecimalFormat

import org.joda.time.format.PeriodFormatterBuilder
import org.joda.time.{DateTimeUtils, Period}

trait ProgressMeter {

  def startRun(fileCount: Int)
  def startFile(name: String)
  def progressInFile(records: Int, bytes: Long)
  def finishFile(records: Int, bytes: Long)
  def finish()
}

object NullProgressMeter extends ProgressMeter {
  override def startRun(fileCount: Int): Unit = {}
  override def startFile(name: String): Unit = {}
  override def progressInFile(records: Int, bytes: Long) = {}
  override def finishFile(records: Int, bytes: Long): Unit = {}
  override def finish(): Unit = {}
}

class ConsoleProgressMeter(val out: PrintStream = System.err) extends ProgressMeter {

  private var totalFiles = 0
  private var filesDone = 0
  private var startTime = 0L
  private var fileStartTime = 0L
  private var timePerFile = 0L
  private var widthOfLastWrite = 0
  private var currentFile = ""
  private val recordsPerMs = new WorkMeter
  private val bytesPerMs = new WorkMeter

  override def startRun(fileCount: Int): Unit = {
    totalFiles = fileCount
    filesDone = 0
    startTime = DateTimeUtils.currentTimeMillis()
    recordsPerMs.start(startTime)
    bytesPerMs.start(startTime)
  }

  override def startFile(name: String): Unit = {
    filesDone += 1
    val now = DateTimeUtils.currentTimeMillis()
    fileStartTime = now
    currentFile = name
    showProgress()
  }

  override def progressInFile(records: Int, bytes: Long) = {
    val now = DateTimeUtils.currentTimeMillis()
    recordsPerMs.workCompleted(records, now)
    bytesPerMs.workCompleted(bytes, now)
    showProgress()
  }

  override def finishFile(records: Int, bytes: Long): Unit = {
    val now = DateTimeUtils.currentTimeMillis()
    val elapsed = Math.max(now - fileStartTime, 1)
    timePerFile = elapsed
    recordsPerMs.workCompleted(records, now)
    bytesPerMs.workCompleted(bytes, now)
  }

  override def finish(): Unit = {
    write(s"\rTook $runTime for ${recordsPerMs.totalCompleted} logs (${recordsPerMs.summary()}/ms) ${bytesPerMs.totalCompleted}B (${bytesPerMs.summary()}B/ms)")
    write("\n")
  }

  private def showProgress(): Unit = {
    val remainingTime = formatRunTime((totalFiles - filesDone) * timePerFile)
    write(s"\rOn $currentFile $filesDone of $totalFiles ($recordsPerMs logs/ms) (${bytesPerMs}B/ms) T+ $runTime T- $remainingTime")
  }

  private def write(s: String) = {
    val padded = if (s.startsWith("\r") && widthOfLastWrite > 0) {
      "%s%s".format(s, " " * Math.max(0, widthOfLastWrite - s.length))
    } else {
      s
    }
    out.print(padded)
    widthOfLastWrite = if (s.endsWith("\n")) 0 else s.length
  }

  private def runTime: String = formatRunTime(DateTimeUtils.currentTimeMillis() - startTime)

  private def formatRunTime(elapsed: Long): String = {
    val formatter = new PeriodFormatterBuilder()
      .appendHours()
      .appendSuffix(" hour", "hours")
      .appendSeparatorIfFieldsBefore(" ")
      .appendMinutes()
      .appendSuffix(" minute", " minutes")
      .appendSeparatorIfFieldsBefore(" ")
      .appendSeconds()
      .appendSuffix("s")
      .toFormatter
    formatter.print(new Period(elapsed))
  }
}

/**
 * Record min,max,avg work per millisecond. Measured over 1 second intervals.
 */
class WorkMeter {
  
  private var work = 0L
  private var rate = 0L
  private var firstTick = 0L
  private var totalWork = 0L
  private var tick = 0L
  private var minRate = Long.MaxValue
  private var maxRate = 0L

  def start(now: Long) = {
    firstTick = now
    tick = firstTick
  }
  
  def workCompleted(w: Long, now: Long) = {
    work += w
    val elapsed = now - tick
    if (elapsed >= 1000) {
      rate = work / elapsed
      tick = now
      totalWork += work
      work = 0
      if (minRate > rate) minRate = rate
      if (maxRate < rate) maxRate = rate
    }
  }
  
  def totalCompleted: String = SIUnitsFormatter.format(totalWork)

  override def toString: String = SIUnitsFormatter.format(rate)

  def summary(): String = {
    val min = if (minRate == Long.MinValue) 0 else minRate
    val max = maxRate
    val elapsed = tick - firstTick
    val avg = if (elapsed == 0) 0 else totalWork / elapsed
    Seq(min, avg, max).map(SIUnitsFormatter.format).mkString(":")
  }
}

object SIUnitsFormatter {

  @inline
  private val unit = 1000
  private val numberFormat = new DecimalFormat("0.#")
  
  def format(value: Long): String = {
    if (value < unit) {
      value.toString
    } else {
      val multipleOf1K = (Math.log(value) / Math.log(unit)).toInt
      val scaled = value / Math.pow(unit, multipleOf1K)
      numberFormat.format(scaled) + "kMGTPE".charAt(multipleOf1K - 1)
    }
  }
}
