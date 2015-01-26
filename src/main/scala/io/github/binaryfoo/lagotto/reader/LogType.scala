package io.github.binaryfoo.lagotto.reader

import java.util

import com.typesafe.config.{ConfigObject, ConfigValue, Config}
import io.github.binaryfoo.lagotto.{SourceRef, LogEntry}

import scala.collection.{mutable, JavaConversions}
import JavaConversions.asScalaBuffer
import JavaConversions.asScalaSet

trait LogType[+T <: LogEntry] extends (SourceLineIterator => T) {
  def canParse(firstLine: String): Boolean = true
  def readLinesForNextRecord(it: SourceLineIterator): LineSet
  def parse(s: LineSet): T
  def apply(it: SourceLineIterator): T = {
    val record = readLinesForNextRecord(it)
    if (record != null) parse(record)
    else null.asInstanceOf[T]
  }
}

case class LineSet(lines: Seq[String], fullText: String, source: SourceRef)

object LogTypes {

  def load(config: Config): Map[String, LogType[LogEntry]] = {
    load(config.getObject("logTypes"))
  }

  def load(types: ConfigObject): Map[String, LogType[LogEntry]] = {
    types.entrySet().map { e =>
      val name = e.getKey
      val v = e.getValue
      val map = v.unwrapped().asInstanceOf[util.Map[String, ConfigValue]]
      val logType = if (map.containsKey("class")) {
        val clazz = map.get("class").asInstanceOf[String]
        val args = asScalaBuffer(map.get("args").asInstanceOf[util.List[String]])
        newInstance(clazz, args)
      } else {
        val clazz = map.get("object").asInstanceOf[String]
        newObject(clazz)
      }
      name -> logType
    }.toMap
  }

  def newInstance(name: String, args: mutable.Buffer[String]): LogType[LogEntry] = {
    val constructor = Class.forName(name).getConstructors()(0)
    val preparedArgs = constructor.getParameterTypes.zip(args).map {
      case (t, v) if t == classOf[String] => v
      case (t, v) if t == classOf[LineRecogniser] => newObject[LineRecogniser](v)
    }
    constructor.newInstance(preparedArgs :_*).asInstanceOf[LogType[LogEntry]]
  }

  def newObject[T](name: String): T = {
    Class.forName(name + "$").getField("MODULE$").get(null).asInstanceOf[T]
  }

  implicit class RichLogTypes(val m: Map[String, LogType[LogEntry]]) extends AnyVal {

    def list(names: Seq[String]): Seq[LogType[LogEntry]] = {
      names.map(m(_))
    }
  }
}
