package io.github.binaryfoo.lagotto.output

/**
 * Why not something like https://github.com/json4s/json4s? ~20mb of dependencies to write a line of JSON.
 * The world doesn't need another json library but...
 */
object DeadSimpleJsonWriter {

  def toJson(data: Map[String, String]): String = {
    val b = new StringBuilder("{")
    var first = true

    data.foreach { case (k, v) =>
      if (!first) b.append(',')
      else first = false
      b.append('"').append(k).append("\":\"").append(v).append('"')
    }
    b.append('}')

    b.toString()
  }
}
