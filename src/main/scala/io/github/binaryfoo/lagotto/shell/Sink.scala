package io.github.binaryfoo.lagotto.shell

import java.io.{File, FileOutputStream, FileWriter, PrintStream, PrintWriter}
import java.net.{HttpURLConnection, Socket, URL}

import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.output.{GnuplotOptions, GnuplotScriptWriter}
import org.HdrHistogram.Histogram

import scala.collection.mutable
import scala.language.postfixOps
import scala.sys.process._

trait Sink {
  def entry(e: LogEntry)
  def finish()
}

/**
 * Write each log record to the console as it pops out of the pipeline.
 */
class IncrementalSink(val format: OutputFormat, val includeHeader: Boolean, val out: PrintStream = Console.out) extends Sink {

  private var headerWritten = false

  override def entry(e: LogEntry): Unit = {
    val output = format(e)
    if (!headerWritten) {
      if (includeHeader) {
        format.header().foreach(out.println)
      }
      headerWritten = true
    }
    output.foreach(out.println)
  }

  override def finish(): Unit = format.footer().foreach(out.println)

}

/**
 * Write each log record to a file as it pops out of the pipeline.
 */
class FileSink(format: OutputFormat, includeHeader: Boolean, val fileName: String) extends IncrementalSink(format, includeHeader, new PrintStream(new FileOutputStream(fileName))) {

  override def finish(): Unit = {
    super.finish()
    out.close()
    println(s"Wrote $fileName")
  }

}

/**
 * Spit out two files: the data (.csv) and a script to plot the series in that file (.gp).
 */
class GnuplotSink(val csvFileName: String, val gpFileName: String, val plotOptions: GnuplotOptions) extends Sink {

  private var xRange = ("", "")

  override def entry(e: LogEntry): Unit = {
    val time = plotOptions.timeField(e)
    if (time != null) {
      xRange = xRange match {
        case ("", _) => (time, time)
        case (start, _) => (start, time)
      }
    }
  }

  override def finish(): Unit = {
    val file = new File(gpFileName)
    val writer = new FileWriter(file)
    writer.write(GnuplotScriptWriter.write(csvFileName, xRange, plotOptions))
    writer.close()
    println(s"Wrote $gpFileName")
    file.setExecutable(true)
    s"/usr/bin/env gnuplot $gpFileName" !
  }

}

class CompositeSink(val sinks: Seq[Sink]) extends Sink {

  override def entry(e: LogEntry): Unit = sinks.foreach(_.entry(e))

  override def finish(): Unit = sinks.foreach(_.finish())

}

/**
 * Write to the console an HDR histogram for the range of
 * values produced by ''field''.
 */
class SingleHistogramSink(val field: FieldExpr) extends Sink {

  private val histogram = new Histogram(3600000000000L, 3)

  override def entry(e: LogEntry): Unit = {
    val v = field(e)
    if (v != null) {
      histogram.recordValue(v.toLong)
    }
  }
  override def finish(): Unit = {
    histogram.outputPercentileDistribution(Console.out, 1.0)
  }

}

/**
 * Output an HDR histogram for each group. The group is identified by ''keyFields''. The histogram is for the range of
 * values produced by ''field''. Each histogram is written to a file name ''prefix''.hgrm where prefix is the group identifier.
 */
class MultipleHistogramSink(val keyFields: Seq[FieldExpr], val field: FieldExpr) extends Sink {

  private val histograms = mutable.Map[String, Histogram]()

  override def entry(e: LogEntry): Unit = {
    val v = field(e)
    if (v != null) {
      val key = e.exprToSeq(keyFields).mkString("-").replace(' ', '-')
      val histogram = histograms.getOrElseUpdate(key, new Histogram(3600000000000L, 3))
      histogram.recordValue(v.toLong)
    }
  }

  override def finish(): Unit = {
    for ((key, histogram) <- histograms) {
      val name = s"$key.hgrm"
      val out = new PrintStream(new FileOutputStream(name))
      histogram.outputPercentileDistribution(out, 1.0)
      out.close()
      println(s"Wrote $name")
    }
  }

}

case class InfluxDBSink(format: OutputFormat, url: String) extends Sink {

  private val buffer = mutable.ArrayBuffer[String]()

  override def entry(e: LogEntry): Unit = {
    buffer ++= format(e)
    if (buffer.size == 5000) {
      post()
    }
  }

  override def finish(): Unit = {
    if (buffer.nonEmpty) {
      post()
    }
  }

  private def post(): Unit = {
    val payload = buffer.mkString("\n").getBytes()

    val connection = new URL(url).openConnection().asInstanceOf[HttpURLConnection]
    connection.setRequestMethod("POST")
    connection.setDoOutput(true)
    connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")
    connection.setRequestProperty("Content-Length", payload.size.toString)
    connection.connect()

    val out = connection.getOutputStream
    out.write(payload)
    out.close()

    val code = connection.getResponseCode
    if (code != 204) {
      stderr.println(s"Failed to POST to $url: $code ${connection.getResponseMessage}")
    }
    connection.disconnect()

    buffer.clear()
  }
}

// Needs attention
case class GraphiteSink(format: OutputFormat, url: String, prefix: String) extends Sink {

  private val IgnoredKeys = Set("datetime", "at")
  private var socket: Option[Socket] = None
  private val out = {
    url match {
      case "-" =>
        new PrintWriter(Console.out)
      case _ =>
        val Array(host, port) = url.split(":")
        socket = Some(new Socket(host, port.toInt))
        new PrintWriter(socket.get.getOutputStream)
    }
  }
  private val exporter: (LogEntry) => Seq[(String, String)] = {
    format match {
      case Tabular(fields, _) =>
        // exclude any time fields, assume they're in the metric's timestamp
        val reducedFields = fields.filterNot(_.isInstanceOf[TimeExpr])
        e: LogEntry => e.exportAsSeq(reducedFields)
      case _ =>
        e: LogEntry => e.exportAsSeq
    }
  }
  private val timeExporter: (LogEntry) => Long = {
    format match {
      case Tabular(fields, _) =>
        // rebuild the time... yikes
        val timeField = fields.find(_.isInstanceOf[TimeExpr]).get.asInstanceOf[TimeExpr]
        e: LogEntry => timeField.formatter.parseDateTime(timeField(e)).getMillis / 1000
      case _ =>
        e: LogEntry => e.timestamp.getMillis / 1000
    }
  }

  override def entry(e: LogEntry): Unit = {
    val time = timeExporter(e)
    for ((key, value) <- exporter(e) if !IgnoredKeys.contains(key)) {
      val cleanKey = prefix + key.replace(' ', '.').replaceAll("[)(]", "_")
      out.println(s"$cleanKey $value $time")
    }
  }

  override def finish(): Unit = {
    out.close()
    socket.foreach(_.close())
  }
}