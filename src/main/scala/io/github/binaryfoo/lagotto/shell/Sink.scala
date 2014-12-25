package io.github.binaryfoo.lagotto.shell

import java.io.{FileOutputStream, PrintStream}

import io.github.binaryfoo.lagotto.LogLike
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