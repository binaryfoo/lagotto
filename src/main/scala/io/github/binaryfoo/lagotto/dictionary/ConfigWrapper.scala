package io.github.binaryfoo.lagotto.dictionary

import com.typesafe.config.{ConfigObject, ConfigUtil, ConfigValue, Config}
import io.github.binaryfoo.lagotto.IAmSorryDave

import scala.collection.JavaConversions.asScalaSet
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.mutable

object ConfigWrapper {

  implicit class RichConfig(val config: Config) extends AnyVal {

    def entries(path: String): mutable.Set[java.util.Map.Entry[String, ConfigValue]] = {
      if (config.hasPath(path)) config.getConfig(path).entries()
      else mutable.Set()
    }

    def entries(): mutable.Set[java.util.Map.Entry[String, ConfigValue]] = {
      val entries: mutable.Set[java.util.Map.Entry[String, ConfigValue]] = config.entrySet()
      entries
    }

    def getOrDie(path: String): Config = {
      checkPath(path)
      config.getConfig(path)
    }

    def getObjectOrDie(path: String): ConfigObject = {
      checkPath(path)
      config.getObject(path)
    }

    def checkPath(path: String): Unit = {
      if (!config.hasPath(path))
        throw new IAmSorryDave(s"Missing key '$path' in file Config from ${config.origin().description()}")
    }
  }

  def toPath(s: String): String = {
    val path: mutable.Buffer[String] = ConfigUtil.splitPath(s)
    path.mkString(".")
  }

}
