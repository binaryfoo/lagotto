package io.github.binaryfoo.lagotto.reader

import java.io._
import java.util.concurrent.ArrayBlockingQueue
import java.util.zip.GZIPInputStream

import io.github.binaryfoo.lagotto._

import scala.annotation.tailrec
import scala.collection.AbstractIterator
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.io.{BufferedSource, Source}
import scala.util.Try

trait SkeletonLogReader[T <: LogEntry] {

  def progressMeter: ProgressMeter

  def readFilesOrStdIn(args: Iterable[String]): Iterator[T] = {
    if (args.isEmpty)
      read(System.in)
    else
      read(args.map(new File(_)))
  }

  def read(files: File*): Iterator[T] = read(files.toIterable)

  def read(files: Iterable[File]): Iterator[T] = {
    progressMeter.startRun(files.size)
    files.toIterator.flatMap(f => read(open(f), f.getName))
  }

  /**
   * @deprecated Use read(InputStream,String) instead.
   */
  def read(source: Source): Iterator[T] = read(source, "")

  /**
   * @deprecated Use read(InputStream,String) instead.
   */
  def read(source: Source, sourceName: String): Iterator[T] = {
    source match {
      case s: BufferedSource =>
        val field = classOf[BufferedSource].getDeclaredField("inputStream")
        field.setAccessible(true)
        read(field.get(s).asInstanceOf[InputStream], sourceName)
    }
  }

  final def read(in: InputStream, sourceName: String = ""): Iterator[T] = {
    readWithProgress(new ProgressInputStream(in, progressMeter, sourceName))
  }

  def readWithProgress(in: ProgressInputStream): Iterator[T]

  private def open(f: File): InputStream = {
    val in = new BufferedInputStream(new FileInputStream(f))
    if (f.getName.endsWith(".gz")) new GZIPInputStream(in)
    else in
  }

}

/**
 * Kicks off a daemon thread to read lines. Parsing is delegated to the default ExecutionContext.
 *
 * @param strict Whinge with an exception on unexpected input
 * @param keepFullText If false keep only the parsed fields. If true keep the full text of every record. Maybe this should be removed.
 * @param logType
 * @tparam T
 */
case class LogReader[T <: LogEntry](strict: Boolean = false, keepFullText: Boolean = true, progressMeter: ProgressMeter = NullProgressMeter, logType: LogType[T] = JposLog) extends SkeletonLogReader[T] {

  override def readWithProgress(source: ProgressInputStream): Iterator[T] = new LogEntryIterator(source)

  private val processors = Runtime.getRuntime.availableProcessors()

  class LogEntryIterator(source: ProgressInputStream) extends AbstractIterator[T] {

    private val queue = new ArrayBlockingQueue[Future[T]](processors * processors * 2)
    private var current: T = null.asInstanceOf[T]
    private var started = false
    private val reader = new Thread(new Runnable {
      private val lines = new LineIterator(source, strict, keepFullText)

      override def run(): Unit = {
        var more = true
        do {
          val f = try {
            val entry = logType.readLinesForNextRecord(lines)
            if (entry != null) {
              parseInTheFuture(entry)
            } else {
              more = false
              Future.successful(null.asInstanceOf[T])
            }
          }
          catch {
            case e: Exception => Future.failed(e)
          }
          queue.put(f)
        } while (more)
      }
    }, s"${source.sourceName}-reader")
    reader.setDaemon(true)

    override def hasNext: Boolean = {
      ensureStarted()
      current != null
    }

    override def next(): T = {
      ensureStarted()
      val v = current
      current = readNext()
      v
    }

    private def readNext(): T = {
      val entry = readNextWithRetry()
      val done = entry == null
      source.publishProgress(done)
      if (done) {
        source.close()
      }
      entry
    }

    @tailrec
    private def readNextWithRetry(): T = {
      val future = queue.take()
      Await.ready(future, 1.minute)
      val maybe = future.value.get
      if (strict && maybe.isFailure) {
        maybe.get
      } else {
        if (maybe.isSuccess) maybe.get else readNextWithRetry()
      }
    }

    @inline
    private def ensureStarted() = {
      if (!started) {
        reader.start()
        started = true
        current = readNext()
      }
    }

    override def foreach[U](f: (T) => U): Unit = {
      try {
        super.foreach(f)
      }
      finally {
        close()
      }
    }

    override def finalize(): Unit = close()

    def close(): Unit = {
      reader.interrupt()
      source.close()
    }

    @inline
    private def parseInTheFuture(entry: logType.P): Future[T] = {
      Future {
        try {
          logType.parse(entry)
        }
        catch {
          case e: Exception => throw new IAmSorryDave(s"Failed record ending at ${entry.source}", e)
        }
      }
    }

  }
}

case class SingleThreadLogReader[T <: LogEntry](strict: Boolean = false, keepFullText: Boolean = true, progressMeter: ProgressMeter = NullProgressMeter, logType: LogType[T] = JposLog) extends SkeletonLogReader[T] {
  override def readWithProgress(source: ProgressInputStream): Iterator[T] = new EntryIterator[T](source, strict, keepFullText, logType)
}

/**
 * Each entry may consume more than one line.
 */
class EntryIterator[T <: LogEntry](val source: ProgressInputStream, val strict: Boolean = false, val keepFullText: Boolean = true, logType: LogType[T] = JposLog) extends AbstractIterator[T] {

  private val lines = new LineIterator(source, strict, keepFullText)
  private var current = readNext()

  override def next(): T = {
    val c = current
    current = readNext()
    c
  }

  override def hasNext: Boolean = current != null

  private def readNext(): T = {
    val next = readNextWithRetry()
    val done = next == null
    source.publishProgress(done)
    if (done)
      source.close()
    next
  }

  @tailrec
  private def readNextWithRetry(): T = {
    val maybe = Try(logType.apply(lines))
    if (strict && maybe.isFailure || maybe.isSuccess) {
      maybe.get
    } else {
      readNextWithRetry()
    }
  }
}

/**
 * Adds a line number, name and a single line push back over Source.getLines().
 */
class LineIterator(in: ProgressInputStream, val strict: Boolean = false, val keepFullText: Boolean = true) extends AbstractIterator[String] with BufferedIterator[String] {

  private val lines = new BufferedReader(new InputStreamReader(in))
  private var linesRead = 0
  private var currentLineNo = 0
  private var current: String = null

  readNext()

  /**
   * Zero when next() has not been called.
   * After next() has been called, the line number for the most recently returned value of next().
   */
  def lineNumber: Int = currentLineNo

  def sourceName: String = in.sourceName

  /**
   * @return Line number and file name for most recently returned value of next().
   */
  def sourceRef: SourceRef = SourceRef(in.sourceName, currentLineNo)

  /**
   * @return Line number and file name for most recently returned value of head.
   */
  def headRef: SourceRef = SourceRef(in.sourceName, linesRead)

  def hasNext = current != null || readNext()

  def next(): String = {
    val c = current
    currentLineNo = linesRead
    readNext()
    c
  }

  def head: String = current

  private def readNext(): Boolean = {
    current = lines.readLine()
    val readOne = current != null
    if (readOne) {
      linesRead += 1
    }
    readOne
  }
}
