package io.github.binaryfoo.lagotto.reader

import java.io._
import java.util.zip.GZIPInputStream

import scala.io.Source

object FileIO {

  def open(f: String): InputStream = open(new File(f))

  def open(f: File): InputStream = {
    val in = new BufferedInputStream(new FileInputStream(f))
    if (f.getName.endsWith(".gz")) new GZIPInputStream(in)
    else in
  }

  def copyLines(file: String, from: Int, to: Option[Int], out: PrintWriter) = {
    val source = Source.fromInputStream(FileIO.open(file))
    try {
      val lines = source.getLines().drop(from)
      for (line <- to.map(t => lines.take(t - from)).getOrElse(lines)) {
        out.println(line)
      }
    }
    finally {
      source.close()
      out.close()
    }
  }
}
