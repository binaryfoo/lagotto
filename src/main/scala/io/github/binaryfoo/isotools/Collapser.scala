package io.github.binaryfoo.isotools

import scala.collection.mutable.ListBuffer

object Collapser {

  def coalesce[T <: Coalesced with ConvertibleToMap](seq: Iterable[T], selector: T => String): Iterable[Coalesced] = {
    val coalesced = new ListBuffer[Coalesced]()
    val group = new ListBuffer[T]()
    var groupKey = ""

    def addEntry() {
      if (group.nonEmpty) {
        coalesced += group.head
        if (group.size > 1) {
          coalesced += Group(group.size - 1, groupKey)
        }
      }
    }

    for (pair <- seq) {
      val key = selector(pair)
      if (key != groupKey) {
        addEntry()
        group.clear()
      }
      group += pair
      groupKey = key
    }
    addEntry()

    coalesced
  }

}

trait Coalesced

case class Group(size: Int, key: String) extends Coalesced
