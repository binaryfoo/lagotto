package io.github.binaryfoo.lagotto.shell

import java.io.{File, FileOutputStream, FileWriter, PrintStream}

import io.github.binaryfoo.lagotto.output.GnuplotScriptWriter
import io.github.binaryfoo.lagotto._
import org.HdrHistogram.Histogram

import scala.collection.mutable

trait Sink {
  def start()
  def entry(e: LogEntry)
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

  override def entry(e: LogEntry) = format(e).foreach(println)

  override def finish() = format.footer().foreach(println)

}

class PivotTableSink(val rotateOn: DirectExpr, val pivot: PivotExpr, val aggregates: Seq[AggregateExpr], val format: TableFormatter, val includeHeader: Boolean) extends Sink {

  private var fields: Seq[String] = Seq()
  private var currentKey: String = null
  private var current: Map[String, Seq[String]] = Map.empty

  override def start() = {}

  override def entry(e: LogEntry) = {
    if (fields.isEmpty) {
      fields = Seq(rotateOn.field) ++ pivot.distinctValues().flatMap(v => aggregates.map(v + " - " + _.field))
      if (includeHeader) {
        format.header(fields).foreach(println)
      }
    }
    val thisKey = rotateOn(e)
    if (thisKey != currentKey) {
      outputRow()
    }
    currentKey = thisKey
    current = current.updated(pivot(e), aggregates.map(_(e)))
  }

  def outputRow() = {
    if (currentKey != null) {
      val row = Seq(currentKey) ++ pivot.distinctValues().flatMap { v =>
        current.getOrElse(v, aggregates.map(_ => "0"))
      }
      format.row(row).foreach(println)
      current = Map.empty
    }
  }

  override def finish() = {
    outputRow()
    format.footer().foreach(println)
  }

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

  override def entry(e: LogEntry) = format(e).foreach(out.println)

  override def finish() = {
    format.footer().foreach(out.println)
    out.close()
    println(s"Wrote $fileName")
  }

}

/**
 * Spit out two files: the data (.csv) and a script to plot the series in that file (.gp).
 */
class GnuplotSink(val fields: Seq[FieldExpr], val csvFileName: String, val gpFileName: String, val baseName: String) extends Sink {

  var xRange = ("", "")
  
  override def start() = {}

  override def entry(e: LogEntry) = {
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
    writer.write(GnuplotScriptWriter.write(fields.map(_.field), csvFileName, baseName, xRange))
    writer.close()
    println(s"Wrote $gpFileName")
    file.setExecutable(true)
  }

}

class CompositeSink(val sinks: Seq[Sink]) extends Sink {

  override def start() = sinks.foreach(_.start())

  override def entry(e: LogEntry) = sinks.foreach(_.entry(e))

  override def finish() = sinks.foreach(_.finish())

}

/**
 * Write to the console an HDR histogram for the range of
 * values produced by ''field''.
 */
class SingleHistogramSink(val field: FieldExpr) extends Sink {

  private val histogram = new Histogram(3600000000000L, 3)

  override def start() = {}

  override def entry(e: LogEntry) = {
    val v = field(e)
    if (v != null) {
      histogram.recordValue(v.toLong)
    }
  }
  override def finish() = {
    histogram.outputPercentileDistribution(Console.out, 1.0)
  }

}

/**
 * Output an HDR histogram for each group. The group is identified by ''keyFields''. The histogram is for the range of
 * values produced by ''field''. Each histogram is written to a file name ''prefix''.hgrm where prefix is the group identifier.
 */
class MultipleHistogramSink(val keyFields: Seq[FieldExpr], val field: FieldExpr) extends Sink {

  private val histograms = mutable.Map[String, Histogram]()

  override def start() = {}

  override def entry(e: LogEntry) = {
    val v = field(e)
    if (v != null) {
      val key = e.exprToSeq(keyFields).mkString("-").replace(' ', '-')
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