package io.github.binaryfoo.lagotto.shell

import java.io.{PrintStream, ByteArrayOutputStream}

import io.github.binaryfoo.lagotto.LogEntry
import org.scalatest.{Matchers, FlatSpec}

class AsciiTableFormatTest extends FlatSpec with Matchers {

  "Ascii table format" should "make a table" in {
    val sink = new AsciiTableFormat()
    val fields = Seq("one", "two")
    sink.header(fields)
    sink.row(fields, LogEntry("one" -> "v1", "two" -> "long v2"))
    sink.row(fields, LogEntry("one" -> "v1 r2", "two" -> "v2 r2"))

    sink.footer().get shouldBe """===================
                            || one   | two     |
                            |===================
                            || v1    | long v2 |
                            || v1 r2 | v2 r2   |
                            |===================
                            |""".stripMargin
  }
}
