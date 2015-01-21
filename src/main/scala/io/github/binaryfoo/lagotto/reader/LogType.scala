package io.github.binaryfoo.lagotto.reader

import java.util

import com.typesafe.config.{ConfigValue, Config}
import io.github.binaryfoo.lagotto.LogEntry

import scala.collection.{mutable, JavaConversions}
import JavaConversions.asScalaBuffer

trait LogType[+T <: LogEntry] extends (SourceLineIterator => T) {
  def canParse(firstLine: String, fileName: String): Boolean = true
}

object LogTypes {

  def load(config: Config): Map[String, LogType[LogEntry]] = {
    load(asScalaBuffer(config.getList("logTypes")))
  }

  def load(types: Seq[ConfigValue]): Map[String, LogType[LogEntry]] = {
    types.map { v =>
      val map = v.unwrapped().asInstanceOf[util.Map[String, ConfigValue]]
      val name = map.get("name").asInstanceOf[String]
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
    constructor.newInstance(args: _*).asInstanceOf[LogType[LogEntry]]
  }

  def newObject(name: String): LogType[LogEntry] = {
    Class.forName(name + "$").getField("MODULE$").get(null).asInstanceOf[LogType[LogEntry]]
  }

  implicit class RichLogTypes(val m: Map[String, LogType[LogEntry]]) extends AnyVal {

    def list(names: Seq[String]): Seq[LogType[LogEntry]] = {
      names.map(m(_))
    }
  }
}
