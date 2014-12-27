package io.github.binaryfoo.lagotto.shell

import java.io.{PrintStream, ByteArrayOutputStream}

import io.github.binaryfoo.lagotto.LogEntry
import org.scalatest.{Matchers, FlatSpec}

class AsciiTableFormatTest extends FlatSpec with Matchers {

  "Ascii table format" should "make a table" in {
    val format = new AsciiTableFormat()
    val fields = Seq("one", "two")
    format.header(fields)
    format.row(Seq("v1", "long v2"))
    format.row(Seq("v1 r2", "v2 r2"))

    format.footer().get shouldBe """===================
                                 || one   | two     |
                                 |===================
                                 || v1    | long v2 |
                                 || v1 r2 | v2 r2   |
                                 |===================
                                 |""".stripMargin
  }

  it should "handle a field name longer than the value" in {
    val format = new AsciiTableFormat()
    val fields = Seq("one", "quite long really")
    format.header(fields)
    format.row(Seq("v1", "v2"))

    format.footer().get shouldBe """===========================
                                 || one | quite long really |
                                 |===========================
                                 || v1  | v2                |
                                 |===========================
                                 |""".stripMargin
  }

  "Incremental ascii table" should "spit out rows as they're added" in {
    val format = new IncrementalAsciiTableFormat()
    val fields = Seq("one", "two")

    val head = format.header(fields).get
    val row1 = format.row(Seq("v1", "fatter")).get
    val row2 = format.row(Seq("fatter", "v2")).get
    val foot = format.footer().get

    head shouldBe """=============
                    || one | two |
                    |=============""".stripMargin
    row1 shouldBe   "| v1  | fatter |"
    row2 shouldBe   "| fatter | v2     |"
    foot shouldBe   "==================="
  }
}
