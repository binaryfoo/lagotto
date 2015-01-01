package io.github.binaryfoo.lagotto

import scala.io.Source

trait TestInput {

  def testFile(f: String): String = s"src/test/resources/$f"

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
