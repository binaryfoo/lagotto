package io.github.binaryfoo.lagotto

import org.scalatest.{BeforeAndAfter, Matchers, FlatSpec}

class LagoTest extends FlatSpec with Matchers with TestInput with BeforeAndAfter {

  after {
    FieldExpr.dictionary = None
  }
}
