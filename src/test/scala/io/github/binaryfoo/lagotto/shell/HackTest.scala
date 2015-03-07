package io.github.binaryfoo.lagotto.shell

import java.io.{File, FileWriter, OutputStream, Writer}
import java.util.concurrent.{Executors, TimeUnit}

import com.etsy.statsd.profiler.profilers.CPUProfiler
import com.etsy.statsd.profiler.reporter.Reporter

object HackTest {

  def main(args: Array[String]) {
    val args = Array[String](
      "--csv",
      "count(exception~bad_),count(exception~bad record)"
    ) ++ new File("/Users/wcurrie/play/logs/se1").listFiles().map(_.getAbsolutePath) ++ new File("/Users/wcurrie/play/logs/se1").listFiles().map(_.getAbsolutePath) ++ new File("/Users/wcurrie/play/logs/se1").listFiles().map(_.getAbsolutePath)

    val profiler = new Profiler("trace.txt")
    profiler.start()

    val start = System.currentTimeMillis()
    Console.withOut(new OutputStream {
      override def write(b: Int): Unit = {}
    }) {
      Main.main(args)
    }
    val elapsed = System.currentTimeMillis() - start
    println(s"$elapsed ms")

    profiler.stop()

    import scala.sys.process._

    val svg = new File("a.svg")
    svg.delete()
    val logger = ProcessLogger(svg)
    "cat trace.txt" #| "../FlameGraph/flamegraph.pl" ! logger
    logger.close()
  }

}

class Profiler(val file: String) {

  val writer = new FileWriter(file)
  val reporter = new FileReporter(writer)
  val profiler = new CPUProfiler(reporter, null, 1, 10)
  val executor = Executors.newSingleThreadScheduledExecutor()
  var alive = true

  def start() = {

    executor.scheduleAtFixedRate(new Runnable {
      override def run(): Unit = profiler.profile()
    }, 0, profiler.getPeriod, profiler.getTimeUnit)
  }

  def stop() = {
    executor.shutdown()
    executor.awaitTermination(2, TimeUnit.SECONDS)
    profiler.flushData()
    writer.close()

    println(s"wrote ${reporter.samples} samples to $file")
  }
}

class FileReporter(val out: Writer) extends Reporter {

  var samples = 0L

  override def recordGaugeValue(s: String, l: Long): Unit = {
    val clean = s.substring("cpu.trace.".length)
    out.write(s"$clean $l\n")
    samples += 1
  }
}
