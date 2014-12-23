package io.github.binaryfoo.lagotto

import scala.collection.{immutable, mutable}

object OrderedGroupBy {

  def groupByOrdered[A, K](v: Traversable[A], f: A => K): immutable.ListMap[K, List[A]] = {
    val newBuilder: () => mutable.Builder[A, List[A]] = () => immutable.List.newBuilder
    groupByOrdered(v, f, newBuilder)
  }

  def groupByOrdered[A, K, Repr](v: Traversable[A], f: A => K, newBuilder: () => mutable.Builder[A, Repr]): immutable.ListMap[K, Repr] = {
    val m = mutable.LinkedHashMap.empty[K, mutable.Builder[A, Repr]]
    for (elem <- v) {
      val key = f(elem)
      val builder = m.getOrElseUpdate(key, newBuilder())
      builder += elem
    }
    val b = immutable.ListMap.newBuilder[K, Repr]
    for ((k, v) <- m)
      b += ((k, v.result()))

    b.result()
  }

  implicit class Implicit[A](val v: Traversable[A]) extends AnyVal {

    def groupByOrdered[K](f: A => K): immutable.ListMap[K, List[A]] = {
      OrderedGroupBy.groupByOrdered(v, f)
    }

    def groupByOrdered[K, Repr](f: A => K, newBuilder: () => mutable.Builder[A, Repr]): immutable.ListMap[K, Repr] = {
      OrderedGroupBy.groupByOrdered(v, f, newBuilder)
    }
  }
}
