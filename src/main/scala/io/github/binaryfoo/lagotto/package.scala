package io.github.binaryfoo

package object lagotto {

  /**
   * Generic parameter allows lambdas passed to LogEntry.toXsv() to access fields specific to a LogEntry.
   */
  type FieldAccessor[T <: LogLike] = T => String

  implicit class TappableStream[A](val s: Stream[A]) extends AnyVal {
    def tap(f: A => Unit): Stream[A] = {
      s.map { e =>
        f(e)
        e
      }
    }
  }
}
