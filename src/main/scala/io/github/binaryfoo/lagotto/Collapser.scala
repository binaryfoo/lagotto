package io.github.binaryfoo.lagotto

import scala.collection.mutable.ListBuffer

object Collapser {

  def coalesce[T <: Coalesced with LogEntry](seq: Iterator[T], selector: T => String): Iterator[Coalesced] = {

    case class CurrentGroup(key: String, members: ListBuffer[T]) {

      def toStream: Stream[Coalesced] = {
        members.size match {
          case 0 => Stream.empty
          case 1 => members.head #:: Stream.empty
          case _ => members.head #:: Group(members.size - 1, key) #:: Stream.empty[Coalesced]
        }
      }

      def +=(member: T) = {
        members += member
        this
      }

    }

    def group(s: Stream[T], current: CurrentGroup): Stream[Coalesced] = {
      s match {
        case pair #:: tail =>
          val key = selector(pair)
          if (key != current.key) {
            current.toStream #::: group(tail, CurrentGroup(key, ListBuffer(pair)))
          } else {
            group(tail, current += pair)
          }
        case _ =>
          current.toStream #::: Stream.empty
      }
    }

    group(seq.toStream, CurrentGroup(null, ListBuffer())).toIterator
  }

}

trait Coalesced

case class Group(size: Int, key: String) extends Coalesced
