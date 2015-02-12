package io.github.binaryfoo

package object lagotto {

  /**
   * Generic parameter allows lambdas passed to LogEntry.toXsv() to access fields specific to a LogEntry.
   */
  type FieldAccessor[T <: LogEntry] = T => String

  implicit class TappableIterator[A](val s: Iterator[A]) extends AnyVal {
    def tap(f: A => Unit): Iterator[A] = {
      s.map { e =>
        f(e)
        e
      }
    }
  }

  def deNull(s: String, default: String = ""): String = if (s == null) default else s

  implicit class DeNullableString(val s: String) extends AnyVal {
    def deNull(default: String = "") = lagotto.deNull(s, default)
  }
}
