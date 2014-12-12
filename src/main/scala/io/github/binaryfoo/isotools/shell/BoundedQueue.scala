package io.github.binaryfoo.isotools.shell

import java.util
import scala.collection.JavaConversions._

/**
 * Keep at most N things.
 */
class BoundedQueue[T](val capacity: Int) extends util.ArrayDeque[T] {

  override def add(e: T): Boolean = {
    if (capacity == 0 || !isEmpty && peek() == e) {
      return false
    }
    if (capacity == size) {
      remove()
    }
    super.add(e)
  }

  /**
   * Return the things in FIFO order and empty the queue.
   */
  def dump(): List[T] = {
    val list = this.toList
    clear()
    list
  }
}
