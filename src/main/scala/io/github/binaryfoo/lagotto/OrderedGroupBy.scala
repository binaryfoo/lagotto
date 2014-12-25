package io.github.binaryfoo.lagotto

import scala.collection.{immutable, mutable}

object OrderedGroupBy {

  def groupByOrdered[A, K](v: Iterator[A], f: A => K): immutable.ListMap[K, List[A]] = {
    def newBuilder(k: K): mutable.Builder[A, List[A]] = immutable.List.newBuilder
    groupByOrdered(v, f, newBuilder)
  }

  def groupByOrdered[A, K, Repr](v: Iterator[A], f: A => K, newBuilder: (K) => mutable.Builder[A, Repr]): immutable.ListMap[K, Repr] = {
    val m = mutable.LinkedHashMap.empty[K, mutable.Builder[A, Repr]]
    for (elem <- v) {
      val key = f(elem)
      val builder = m.getOrElseUpdate(key, newBuilder(key))
      builder += elem
    }
    val b = immutable.ListMap.newBuilder[K, Repr]
    for ((k, v) <- m)
      b += ((k, v.result()))

    b.result()
  }

}
