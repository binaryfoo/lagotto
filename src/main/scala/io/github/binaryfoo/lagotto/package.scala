package io.github.binaryfoo

package object lagotto {

  type LogFieldExpr = LogLike => String

  implicit class TappableStream[A](val s: Stream[A]) extends AnyVal {
    def tap(f: A => Unit): Stream[A] = {
      s.map { e =>
        f(e)
        e
      }
    }
  }
}
