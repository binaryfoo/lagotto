package io.github.binaryfoo.lagotto

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

object OrderedGroupBy {

  /**
   * Create groups of A's using f as the grouping function. Outputs one Repr for each group.
   *
   * @param v The elements to group.
   * @param f The key to uniquely identify a group.
   * @param newBuilder Create a new builder for each group.
   */
  def groupByOrdered[A, K, Repr](v: Iterator[A], f: A => K, newBuilder: (K) => mutable.Builder[A, Repr]): Seq[Repr] = {
    val m = mutable.LinkedHashMap.empty[K, mutable.Builder[A, Repr]]
    for (elem <- v) {
      val key = f(elem)
      val builder = m.getOrElseUpdate(key, newBuilder(key))
      builder += elem
    }
    val b = new ArrayBuffer[Repr]()
    b.sizeHint(m.size)
    for ((k, v) <- m)
      b += v.result()

    b.result()
  }

}
