package io.github.binaryfoo.isotools.shell

import java.util

class BoundedQueue[T](val capacity: Int) extends util.ArrayDeque[T] {

  override def add(e: T): Boolean = {
    if (capacity == 0 || !isEmpty && peek() == e) {
      return false
    }
    if (capacity == size()) {
      remove()
    }
    super.add(e)
  }
}
