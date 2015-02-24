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

  def readToString(f: String): String = {
    val source = Source.fromFile(f)
    try {
      source.mkString
    }
    finally {
      source.close()
    }
  }

  def readLines(f: String, from: Int, to: Option[Int] = None): String = {
    val out = new ByteArrayOutputStream()
    copyLines(f, Some(from), to, new PrintWriter(out))
    out.toString
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

  def copyLines(file: String, from: Option[Int], to: Option[Int], out: PrintWriter) = {
    val source = Source.fromInputStream(FileIO.open(file))
    try {
      val lines = if (from.isDefined) {
        val f = from.get
        val head = source.getLines().drop(f)
        to.map(t => head.take(t - f)).getOrElse(head)
      } else {
        source.getLines()
      }
      for (line <- lines) {
        out.println(line)
      }
    }
    finally {
      source.close()
      out.close()
    }
  }
}
