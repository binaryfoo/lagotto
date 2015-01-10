package io.github.binaryfoo.lagotto.output

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.nio.file.Files
import java.util.zip.{GZIPOutputStream, GZIPInputStream}
import javax.xml.bind.DatatypeConverter

import scala.io.Source

object GZip {

  // See java.util.zip.GZIPInputStream.GZIP_MAGIC
  private val GzipMagic = "1F8B"

  def unzip(hex: String): String = {
    try {
      val in = new GZIPInputStream(new ByteArrayInputStream(DatatypeConverter.parseHexBinary(dropHeader(hex))))
      val source = Source.fromInputStream(in)
      try {
        source.mkString
      }
      finally {
        source.close()
      }
    }
    catch {
      case e: Exception => s"Not gzip: $hex"
    }
  }

  def zip(s: String): String = {
    val out = new ByteArrayOutputStream()
    val zip = new GZIPOutputStream(out)
    zip.write(s.getBytes)
    zip.close()
    DatatypeConverter.printHexBinary(out.toByteArray)
  }

  private def dropHeader(hex: String): String = {
    hex.substring(hex.indexOf(GzipMagic))
  }

}
