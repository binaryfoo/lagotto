package io.github.binaryfoo.lagotto.output

/**
 * Why not something like https://github.com/json4s/json4s? ~20mb of dependencies to write a line of JSON.
 * The world doesn't need another json library but...
 */
object DeadSimpleJsonWriter {

  def toJson(data: Map[String, String]): String = {
    val w = new DeadSimpleJsonWriter()
    data.foreach { case (k, v) => w.add(k, v) }
    w.done()
    w.toString()
  }
}

/**
 * Not quite as simple as where it started...
 */
class DeadSimpleJsonWriter() {

  private val b = new StringBuilder("{")
  private var first = true
  
  type ValueAppender = (Any) => Unit
  
  private val stringAppender: ValueAppender = b.append('"').append(_).append('"')
  private val rawAppender: ValueAppender = b.append(_)

  def addAsInt(key: String, value: String): DeadSimpleJsonWriter = add(key, value.toLong.toString, rawAppender)
  def add(key: String, value: String): DeadSimpleJsonWriter = add(key, value, stringAppender)
  def add(key: String, value: Long): DeadSimpleJsonWriter = add(key, value, rawAppender)
  def add(key: String, value: Seq[String]): DeadSimpleJsonWriter = add(key, value.mkString("[", ",", "]"), rawAppender)

  private def add(key: String, value: Any, appender: ValueAppender): DeadSimpleJsonWriter = {
    if (!first) b.append(',')
    else first = false
    b.append('"').append(key).append("\":")
    appender(value)
    this
  }

  def done(): DeadSimpleJsonWriter = {
    b.append('}')
    this
  }

  override def toString: String = b.toString()
}
