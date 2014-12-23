package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

case class AggregateLogLike(key: Map[String, String], values: List[LogLike]) extends LogLike {

  override def timestamp: DateTime = values.head.timestamp

  override def lines: String =  values.map(_.lines).mkString("<group>\n", "\n", "\n</group>")

  override def apply(id: String): String = key.getOrElse(id, id match {
    case "count" => values.size.toString
    case _ => null
  })

}

object AggregateLogLike {
  def aggregate(s: Stream[LogLike], outputFields: Seq[String]): Stream[LogLike] = {
    val keyFields = outputFields.filterNot(_ == "count")
    def keyFor(e: LogLike): Seq[(String, String)] = {
      for {
        k <- keyFields
      } yield (k, e(k))
    }
    OrderedGroupBy.groupByOrdered(s, keyFor).map {
      case (key, values) => AggregateLogLike(key.toMap, values)
    }.toStream
  }

}