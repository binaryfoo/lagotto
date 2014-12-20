package io.github.binaryfoo.lagotto.mmap

import java.nio.ByteBuffer

import scala.collection.immutable.StringLike

/**
 * Odd (and probably failed) attempt at processing records faster by using a memory mapped buffer.
 */
case class LiteString(buf: ByteBuffer, start: Int = 0, override val length: Int) extends CharSequence {

  val absoluteEnd = start + length - 1

  override def charAt(index: Int): Char = {
    if (index < 0 || index >= length)
      throw new IllegalArgumentException(s"Invalid index $index. Not in [0-$length)")
    buf.get(start + index).asInstanceOf[Char]
  }

  override def subSequence(start: Int, end: Int): LiteString = LiteString(buf, this.start + start, end - start)

  override def toString: String = {
    buf.synchronized {
      val bytes = new Array[Byte](length)
      buf.position(start)
      buf.get(bytes)
      new String(bytes)
    }
  }

  def substring(start: Int, end: Int): LiteString = subSequence(start, end)

  def substring(start: Int): LiteString = subSequence(start, length)

  def contains(s: CharSequence): Boolean = indexOf(s) >= 0

  def indexOf(s: CharSequence): Int = indexOf(s, 0)

  def indexOf(s: CharSequence, from: Int): Int = {
    def source(i: Int) = buf.get(i)
    def target(i: Int) = s.charAt(i).asInstanceOf[Byte]

    val first = target(0)
    var i = start + from
    def regionMatch(i: Int): Boolean = {
      var j = i + 1
      var k = 1
      while (j <= absoluteEnd && k < s.length) {
        if (source(j) != target(k)) {
          return false
        }
        j += 1
        k += 1
      }
      k == s.length
    }

    while (i <= absoluteEnd) {
      while (i < absoluteEnd && source(i) != first) {
        i += 1
      }
      if (i <= absoluteEnd) {
        if (regionMatch(i)) {
          return i - start
        }
      }
      i += 1
    }
    -1
  }

  override def equals(other: Any): Boolean = other match {
    case o: CharSequence =>
      val i = indexOf(o)
      i == 0 && o.length() == length
    case _ => false
  }

  def indexOf(c: Char, from: Int = 0): Int = {
    val i = start + from
    if (i > absoluteEnd) {
      -1
    } else if (buf.get(i) == c) {
      from
    } else {
      indexOf(c, from + 1)
    }
  }

  def split(c: Char, from: Int = 0): Stream[LiteString] = {
    val i = indexOf(c, from)
    if (i == -1) {
      subSequence(from, length) #:: Stream.empty
    } else {
      subSequence(from, i) #:: split(c, i + 1)
    }
  }

  def unapply(s: LiteString): Option[LiteString] = {
    if (s != null && s.equals(this))
      Option(s)
    else
      None
  }
}

object LiteString {

  def untilEnd(buf: ByteBuffer, start: Int = 0) = {
    val length = buf.limit() - start
    LiteString(buf, start, length)
  }

  def lite(s: String) = untilEnd(ByteBuffer.wrap(s.getBytes))

}
