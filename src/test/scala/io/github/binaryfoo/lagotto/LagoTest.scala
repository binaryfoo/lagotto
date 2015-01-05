package io.github.binaryfoo.lagotto

import org.scalatest.{FlatSpec, Matchers}

class LagoTest extends FlatSpec with Matchers with TestInput {

  def iteratorOver[T](e: T*): Iterator[T] = List(e :_*).iterator
}
