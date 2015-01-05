package io.github.binaryfoo

package object lagotto {

  /**
   * Generic parameter allows lambdas passed to LogEntry.toXsv() to access fields specific to a LogEntry.
   */
  type FieldAccessor[T <: LogLike] = T => String

  implicit class TappableIterator[A](val s: Iterator[A]) extends AnyVal {
    def tap(f: A => Unit): Iterator[A] = {
      s.map { e =>
        f(e)
        e
      }
    }
  }
}
