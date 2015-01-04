package io.github.binaryfoo.lagotto

import scala.io.Source

trait TestInput {

  def testFile(f: String): String = s"src/test/resources/$f"

  def sourceFrom(f: String): Source = Source.fromFile(testFile(f))

  def linesFrom(f: String): Seq[String] = sourceFrom(f).getLines().toSeq

  def contentsOf(f: String): String = {
    val source = Source.fromFile(testFile(f))
    try {
      source.mkString
    }
    finally {
      source.close()
    }
  }
}
