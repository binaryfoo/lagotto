package io.github.binaryfoo.lagotto

import java.io.PrintStream

import org.joda.time.{Period, DateTimeUtils}
import org.joda.time.format.PeriodFormatterBuilder

trait ProgressMeter {
  def startRun(fileCount: Int)
  def startFile(name: String)
  def finishFile(records: Int)
  def finish()
}

object NullProgressMeter extends ProgressMeter {
  override def startRun(fileCount: Int): Unit = {}
  override def finish(): Unit = {}
  override def finishFile(records: Int): Unit = {}
  override def startFile(name: String): Unit = {}
}

class ConsoleProgressMeter(val out: PrintStream = System.err) extends ProgressMeter {

  private var totalFiles = 0
  private var filesDone = 0
  private var recordsDone = 0
  private var startTime = 0L
  private var fileStartTime = 0L
  private var recordsPerSecond = 0L
  private var widthOfLastWrite = 0

  override def startRun(fileCount: Int): Unit = {
    totalFiles = fileCount
    filesDone = 0
    recordsDone = 0
    recordsPerSecond = 0
    startTime = DateTimeUtils.currentTimeMillis()
  }


  override def startFile(name: String): Unit = {
    filesDone += 1
    fileStartTime = DateTimeUtils.currentTimeMillis()
    val progress = s"\rOn $name $filesDone of $totalFiles $recordsDone entries ($recordsPerSecond/ms) T $runTime"
    widthOfLastWrite = progress.length
    out.print(progress)
  }

  override def finishFile(records: Int): Unit = {
    val elapsed = Math.max(DateTimeUtils.currentTimeMillis() - fileStartTime, 1)
    recordsDone += records
    recordsPerSecond = records / elapsed
  }

  override def finish(): Unit = {
    val took = runTime
    val padding = " " * Math.max(0, widthOfLastWrite - took.length - 5)
    out.printf("\rTook %s%s\n", padding, took)
  }

  def runTime: String = {
    val elapsed = DateTimeUtils.currentTimeMillis() - startTime
    val formatter = new PeriodFormatterBuilder()
      .appendMinutes()
      .appendSuffix(" minute ", " minutes ")
      .appendSecondsWithOptionalMillis()
      .appendSuffix("s")
      .toFormatter
    formatter.print(new Period(elapsed))
  }
}
