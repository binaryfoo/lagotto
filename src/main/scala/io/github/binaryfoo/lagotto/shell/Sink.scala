package io.github.binaryfoo.lagotto.shell

import java.io.{File, FileWriter, FileOutputStream, PrintStream}

import io.github.binaryfoo.lagotto.LogLike
import io.github.binaryfoo.lagotto.gnuplot.GnuplotScriptAuthor
import org.HdrHistogram.Histogram
import scala.collection.mutable

trait Sink {
  def start()
  def entry(e: LogLike)
  def finish()
}

class IncrementalSink(val format: OutputFormat, val includeHeader: Boolean) extends Sink {

  override def start() = {
    if (includeHeader) {
      format.header().foreach(println(_))
    }
  }

  override def entry(e: LogLike) = println(format(e))

  override def finish() = format.footer().foreach(println(_))

}

class FileSink(val format: OutputFormat, val includeHeader: Boolean, val fileName: String) extends Sink {

  val out = new PrintStream(new FileOutputStream(fileName))

  override def start() = {
    if (includeHeader) {
      format.header().foreach(out.println)
    }
  }

  override def entry(e: LogLike) = out.println(format(e))

  override def finish() = {
    format.footer().foreach(out.println)
    out.close()
    println(s"Wrote $fileName")
  }

}

class GnuplotSink(val fields: Seq[String], val csvfileName: String, val gpFileName: String, val baseName: String) extends Sink {

  var xRange = ("", "")
  
  override def start() = {}

  override def entry(e: LogLike) = {
    val time = e(fields.head)
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
    writer.write(GnuplotScriptAuthor.write(fields, csvfileName, baseName, xRange))
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