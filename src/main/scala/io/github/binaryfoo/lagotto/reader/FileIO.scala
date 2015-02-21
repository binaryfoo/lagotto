package io.github.binaryfoo.lagotto.reader

import java.io._
import java.util.zip.GZIPInputStream

import scala.io.Source

object FileIO {

  private val bufferSize = 64 * 1024

  def open(f: String): InputStream = open(new File(f))

  def open(f: File): InputStream = {
    val in = new BufferedInputStream(new FileInputStream(f))
    if (f.getName.endsWith(".gz")) new GZIPInputStream(in)
    else in
  }

  def copy(src: InputStream, dest: OutputStream) = {
    try {
      val buffer = new Array[Byte](bufferSize)
      var len = 0
      do {
        len = src.read(buffer, 0, bufferSize)
        if (len > 0) {
          dest.write(buffer, 0, len)
          dest.flush()
        }
      } while (len >= 0)
    }
    finally {
      src.close()
    }
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
