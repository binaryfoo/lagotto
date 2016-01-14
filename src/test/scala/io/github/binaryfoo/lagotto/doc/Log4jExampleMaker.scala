package io.github.binaryfoo.lagotto.doc

import java.io.{FileOutputStream, PrintWriter, File}
import java.util.zip.GZIPOutputStream

import org.joda.time.DateTime

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/**
  * Create some dummy log4j data for a documentation example.
  */
object Log4jExampleMaker {

  val words = Source.fromFile(new File("/usr/share/dict/words")).getLines().filter(_.length > 6).toSeq
  val levels = Seq("INFO", "ERROR")
  val random = new Random()

  def main(args: Array[String]) {
    val messages = for {
      sample <- 1 to 1000
      level = levels.randomEntry()
      millisecondInDay = random.nextInt(3600 * 24 * 1000)
      sentenceLength = random.nextInt(5) + 3
      message = pickWords(sentenceLength).mkString(" ")
    } yield (millisecondInDay, level, message)

    val w = new PrintWriter(new GZIPOutputStream(new FileOutputStream("log_samples/log4j.txt.gz")))
    for ((time, level, message) <- messages.sortBy(_._1)) {
      // [08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful
      w.println(s"[${new DateTime(time).toString("dd MMM yyyy HH:mm:ss,SSS")}] $level [a.ClassName]: $message")
    }
    w.close()
  }

  implicit class RandomFrom(val seq: Seq[String]) extends AnyVal {
    def randomEntry(): String = {
      seq(random.nextInt(seq.length))
    }
  }

  @tailrec
  def pickWords(n: Int, accumulator: Seq[String] = Seq.empty): Seq[String] = {
    if (n == 0) {
      accumulator
    } else {
      pickWords(n - 1, accumulator :+ words.randomEntry())
    }
  }
}
