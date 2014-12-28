package io.github.binaryfoo.lagotto.shell

import java.io.{File, FileOutputStream, FileWriter, PrintStream}

import io.github.binaryfoo.lagotto.{LogFieldExpr, LogLike}
import io.github.binaryfoo.lagotto.gnuplot.GnuplotScriptAuthor
import org.HdrHistogram.Histogram

import scala.collection.mutable

trait Sink {
  def start()
  def entry(e: LogLike)
  def finish()
}

/**
 * Write each log record to the console as it pops out of the pipeline.
 */
class IncrementalSink(val format: OutputFormat, val includeHeader: Boolean) extends Sink {

  override def start() = {
    if (includeHeader) {
      format.header().foreach(println)
    }
  }

  override def entry(e: LogLike) = format(e).foreach(println)

  override def finish() = format.footer().foreach(println)

}

/**
 * Write each log record to a file as it pops out of the pipeline.
 */
class FileSink(val format: OutputFormat, val includeHeader: Boolean, val fileName: String) extends Sink {

  val out = new PrintStream(new FileOutputStream(fileName))

  override def start() = {
    if (includeHeader) {
      format.header().foreach(out.println)
    }
  }

  override def entry(e: LogLike) = format(e).foreach(out.println)

  override def finish() = {
    format.footer().foreach(out.println)
    out.close()
    println(s"Wrote $fileName")
  }

}

/**
 * Spit out two files: the data (.csv) and a script to plot the series in that file (.gp).
 */
class GnuplotSink(val fields: Seq[LogFieldExpr], val csvFileName: String, val gpFileName: String, val baseName: String) extends Sink {

  var xRange = ("", "")
  
  override def start() = {}

  override def entry(e: LogLike) = {
    val time = fields.head(e)
    if (time != null) {
      xRange = xRange match {
        case ("", _) => (time, time)
        case (start, _) => (start, time)
      }
    }
  }

  override def finish() = {
    val file = new File(gpFileName)
    val writer = new FileWriter(file)
    writer.write(GnuplotScriptAuthor.write(fields.map(_.toString), csvFileName, baseName, xRange))
    writer.close()
    println(s"Wrote $gpFileName")
    file.setExecutable(true)
  }

}

class CompositeSink(val sinks: Seq[Sink]) extends Sink {

  override def start() = sinks.foreach(_.start())

  override def entry(e: LogLike) = sinks.foreach(_.entry(e))

  override def finish() = sinks.foreach(_.finish())

}

/**
 * Write to the console an HDR histogram for the range of
 * values produced by ''field''.
 */
class SingleHistogramSink(val field: String) extends Sink {

  private val histogram = new Histogram(3600000000000L, 3)

  override def start() = {}

  override def entry(e: LogLike) = {
    val v = e(field)
    if (v != null) {
      histogram.recordValue(v.toLong)
    }
  }
  override def finish() = {
    histogram.outputPercentileDistribution(System.out, 1.0)
  }

}

/**
 * Output an HDR histogram for each group. The group is identified by ''keyFields''. The histogram is for the range of
 * values produced by ''field''. Each histogram is written to a file name ''prefix''.hgrm where prefix is the group identifier.
 */
class MultipleHistogramSink(val keyFields: Seq[String], val field: String) extends Sink {

  private val histograms = mutable.Map[String, Histogram]()

  override def start() = {}

  override def entry(e: LogLike) = {
    val v = e(field)
    if (v != null) {
      val key = e.toXsv("-", keyFields)
      val histogram = histograms.getOrElseUpdate(key, new Histogram(3600000000000L, 3))
      histogram.recordValue(v.toLong)
    }
  }

  override def finish() = {
    for ((key, histogram) <- histograms) {
      val name = s"$key.hgrm"
      val out = new PrintStream(new FileOutputStream(name))
      histogram.outputPercentileDistribution(out, 1.0)
      out.close()
      println(s"Wrote $name")
    }
  }

}