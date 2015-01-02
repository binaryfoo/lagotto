package io.github.binaryfoo.lagotto.dictionary

import com.typesafe.config.{ConfigUtil, ConfigValue, Config}

import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable

object ConfigWrapper {

  def entries(config: Config, path: String): mutable.Set[java.util.Map.Entry[String, ConfigValue]] = {
    if (config.hasPath(path)) entries(config.getConfig(path))
    else mutable.Set()
  }

  def entries(config: Config): mutable.Set[java.util.Map.Entry[String, ConfigValue]] = {
    val entries: mutable.Set[java.util.Map.Entry[String, ConfigValue]] = config.entrySet()
    entries
  }

  def toPath(s: String): String = {
    val path: mutable.Buffer[String] = ConfigUtil.splitPath(s)
    path.mkString(".")
  }

}
