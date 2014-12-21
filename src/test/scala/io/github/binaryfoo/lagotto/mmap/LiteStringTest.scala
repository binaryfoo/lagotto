package io.github.binaryfoo.lagotto.mmap

import java.nio.ByteBuffer
import java.nio.charset.Charset

import org.scalatest.{Matchers, FlatSpec}

class LiteStringTest extends FlatSpec with Matchers {

  val buffer = wrap("some contents with more")

  "Lite string" should "use byte buffer" in {
    val contents = LiteString(buffer, 5, 8)
    contents.charAt(0) shouldEqual 'c'
    contents.charAt(7) shouldEqual 's'
    contents.toString shouldEqual "contents"
  }

  it should "reject attempts to read out of bounds" in {
    val contents = LiteString(buffer, 5, 8)
    an [IllegalArgumentException] should be thrownBy {
      contents.charAt(-1)
    }
    an [IllegalArgumentException] should be thrownBy {
      contents.charAt(8)
    }
  }

  it should "return a subsequence backed by the original buffer" in {
    val original = LiteString(buffer, 19, 4)
    val copy = original.subSequence(0, 4)
    copy.toString shouldEqual "more"

    buffer.position(19)
    buffer.put("less".getBytes)

    copy.toString shouldEqual "less"
  }

  "indexOf" should "return index of match" in {
    val s = LiteString.untilEnd(wrap("quick fox"))

    s indexOf ' ' shouldEqual 5
    s indexOf 'y' shouldEqual -1
  }

  it should "only consider mapped region" in {
    val s = LiteString(wrap("a b a"), 1, 3)

    s indexOf 'a' shouldEqual -1
  }

  "split()" should "split on newline" in {
    val lines = LiteString.untilEnd(wrap("nope\none\ntwo\nthree"), 5).split('\n')
    lines.head.toString shouldEqual "one"
    lines.drop(1).head.toString shouldEqual "two"
    lines.drop(2).head.toString shouldEqual "three"
    lines.drop(3) shouldEqual Stream.empty
  }

  it should "return whole string if no match" in {
    val s = LiteString.untilEnd(wrap("crowded")).split(' ')
    s.head.toString shouldEqual "crowded"
    s.tail shouldEqual Stream.empty
  }

  it should "return empty string two consecutive tokens" in {
    val s = LiteString.untilEnd(wrap("\n")).split('\n')
    s.toList.map(_.toString) shouldEqual List("", "")
  }

  "contains()" should "find a match" in {
    val s = LiteString.untilEnd(wrap("quick brown fox"))
    s contains "fox" shouldEqual true
    s contains "pox" shouldEqual false
  }

  it should "only consider mapped region" in {
    val s = LiteString(wrap("fox sox fox"), 4, 4)
    s contains "fox" shouldEqual false
    s contains "sox" shouldEqual true
  }

  def wrap(s: String): ByteBuffer = {
    ByteBuffer.wrap(s.getBytes(Charset.forName("UTF-8")))
  }
}
