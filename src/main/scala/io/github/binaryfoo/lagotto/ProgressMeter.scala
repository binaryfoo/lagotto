package io.github.binaryfoo.lagotto

import java.io.PrintStream

import org.joda.time.{Period, DateTimeUtils}
import org.joda.time.format.PeriodFormatterBuilder

trait ProgressMeter {
  def startRun(fileCount: Int)
  def startFile(name: String)
  def progressInFile(records: Int)
  def finishFile(records: Int)
  def finish()
}

object NullProgressMeter extends ProgressMeter {
  override def startRun(fileCount: Int): Unit = {}
  override def startFile(name: String): Unit = {}
  override def progressInFile(records: Int) = {}
  override def finishFile(records: Int): Unit = {}
  override def finish(): Unit = {}
}

class ConsoleProgressMeter(val out: PrintStream = System.err) extends ProgressMeter {

  private var totalFiles = 0
  private var filesDone = 0
  private var recordsDone = 0
  private var startTime = 0L
  private var fileStartTime = 0L
  private var lastIncrement = 0L
  private var timePerFile = 0L
  private var recordsPerSecond = 0L
  private var maxRecordsPerSecond = 0L
  private var minRecordsPerSecond = Long.MaxValue
  private var widthOfLastWrite = 0
  private var currentFile = ""

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
    lastIncrement = DateTimeUtils.currentTimeMillis()
    currentFile = name
    showProgress()
  }

  override def progressInFile(records: Int) = {
    val now = DateTimeUtils.currentTimeMillis()
    val elapsed = Math.max(now - lastIncrement, 1)
    lastIncrement = now
    recordsDone(records, elapsed)
    showProgress()
  }

  override def finishFile(records: Int): Unit = {
    val elapsed = Math.max(DateTimeUtils.currentTimeMillis() - fileStartTime, 1)
    timePerFile = elapsed
    recordsDone(records, elapsed)
  }

  def recordsDone(records: Int, elapsed: Long): Unit = {
    recordsDone += records
    recordsPerSecond = records / elapsed
    maxRecordsPerSecond = math.max(recordsPerSecond, maxRecordsPerSecond)
    minRecordsPerSecond = math.min(recordsPerSecond, minRecordsPerSecond)
  }

  override def finish(): Unit = {
    val elapsed = totalElapsed
    val recordsPerSecond = recordsDone / elapsed
    val runTime = formatRunTime(elapsed)
    write(s"\rTook $runTime for $recordsDone logs ($minRecordsPerSecond:$recordsPerSecond:$maxRecordsPerSecond/ms)")
    write("\n")
  }

  private def showProgress(): Unit = {
    val remainingTime = formatRunTime((totalFiles - filesDone) * timePerFile)
    write(s"\rOn $currentFile $filesDone of $totalFiles ($recordsPerSecond logs/ms) T+ $runTime T- $remainingTime")
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

  private def runTime: String = formatRunTime(totalElapsed)

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

  private def totalElapsed: Long = DateTimeUtils.currentTimeMillis() - startTime
}
