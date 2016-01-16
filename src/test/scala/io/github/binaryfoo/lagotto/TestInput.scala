package io.github.binaryfoo.lagotto

import java.io.{FileOutputStream, FileInputStream, File}
import java.nio.file.Files

import io.github.binaryfoo.lagotto.reader.FileIO

import scala.io.Source

trait TestInput {

  def testFile(f: String): String = s"src/test/resources/$f"

  def testFiles(names: String*): Iterable[File] = names.map(f => new File(testFile(f)))

  def sourceFrom(f: String): Source = Source.fromFile(testFile(f))

  def linesFrom(f: String): Seq[String] = sourceFrom(f).getLines().toSeq

  def contentsOf(f: String): String = {
    import io.github.binaryfoo.lagotto.TestInput.RichFile
    new File(testFile(f)).readToString()
  }

  def tempFileContaining(content: String): String = {
    val temp = tempFile()
    Files.write(temp.toPath, content.getBytes())
    temp.getAbsolutePath
  }

  def tempFile() = {
    val file = File.createTempFile("lago-test", ".txt", new File("."))
    file.deleteOnExit()
    file
  }

  def copyFile(src: String, dest: String) = {
    val in = new FileInputStream(src)
    val out = new FileOutputStream(dest, true)
    try {
      FileIO.copy(in, out)
    }
    finally {
      out.close()
    }
  }

  def delete(f: String) = Files.deleteIfExists(new File(f).toPath)
}

object TestInput {

  implicit class RichFile(val file: File) extends AnyVal {

    def readToString(): String = {
      val source = Source.fromFile(file)
      try {
        source.mkString
      }
      finally {
        source.close()
      }
    }

    def withSuffix(suffix: String): File = new File(file.getAbsolutePath + "" + suffix)
  }
}
