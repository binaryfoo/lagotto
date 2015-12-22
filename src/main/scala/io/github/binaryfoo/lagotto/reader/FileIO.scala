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

  def readToString(i: InputStream): String = {
    val source = Source.fromInputStream(i)
    try {
      source.mkString
    }
    finally {
      source.close()
    }
  }

  def readLines(f: String, from: Int = 0, to: Option[Int] = None): String = {
    val out = new ByteArrayOutputStream()
    copyLines(f, Some(from), to, new PrintWriter(out))
    out.toString
  }

  def writeLines(f: String, lines: Iterable[String]) = {
    val writer = new PrintWriter(f)
    try {
      lines.foreach(writer.println)
    }
    finally {
      writer.close()
    }
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

  def copy(src: Reader, dest: Writer) = {
    try {
      val buffer = new Array[Char](bufferSize)
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

  def copyLines(file: String, from: Option[Int] = None, to: Option[Int] = None, out: PrintWriter) = {
    val source = Source.fromInputStream(FileIO.open(file))
    try {
      for (line <- slice(source.getLines(), from, to)) {
        out.println(line)
      }
    }
    finally {
      source.close()
      out.close()
    }
  }

  def slice(it: Iterator[String], from: Option[Int], to: Option[Int]): Iterator[String] = {
    (from, to) match {
      case (Some(f), Some(t)) => it.slice(f, t)
      case (Some(f), None) => it.drop(f)
      case (None, None) => it
    }
  }

  def dropSuffix(fileName: String): String = {
    new File(fileName).getName.replaceFirst("\\.[^.]+$", "")
  }
}
