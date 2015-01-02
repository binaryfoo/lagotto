package io.github.binaryfoo.lagotto.dictionary

import java.util.Map.Entry

import com.typesafe.config.{Config, ConfigFactory, ConfigUtil, ConfigValue}
import io.github.binaryfoo.lagotto.LogLike
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType

import scala.collection.JavaConversions.{asScalaBuffer, asScalaSet}
import scala.collection.mutable

/**
 * Field number to human name translation.
 */
object DataDictionary {

  private val config = ConfigFactory.load()

  val englishNames = loadEnglishNames(config)
  lazy val exportNames = englishNames.mapValues(CamelCase.toCamelCase)
  val shortNames = loadShortNames(config)
  val types = loadTypes(config)

  /*
  Needs:
    1. name fields
    2. name values in fields: MTI, NMIC
    3. override defaults based on realm, mti, nmic
   */

  // falls back to short name
  def englishNameOf(field: String, context: LogLike): Option[String] = {
    englishNames.get(field).orElse(shortNameOf(field, context))
  }

  def shortNameOf(field: String, context: LogLike): Option[String] = {
    shortNames.get(field)
  }

  // short name, field name camel cased, or just field
  def exportNameOf(field: String, context: LogLike): String = {
    shortNames.get(field)
      .orElse(exportNames.get(field))
      .getOrElse(field)
  }

  def typeOf(field: String, context: LogLike): FieldType = {
    types.getOrElse(field, FieldType.String)
  }

  private def entries(config: Config, path: String): mutable.Set[java.util.Map.Entry[String, ConfigValue]] = {
    val entries: mutable.Set[java.util.Map.Entry[String, ConfigValue]] = config.getConfig(path).entrySet()
    entries
  }

  private def loadEnglishNames(config: Config): Map[String, String] = {
    val fields = entries(config, "dictionary.global.fields")
    val subfields = entries(config, "dictionary.global.subfields")
    (fields ++ subfields).map(toPair).toMap
  }

  private def loadShortNames(config: Config): Map[String, String] = {
    entries(config, "dictionary.global.shortNames").map(toPair).toMap
  }

  private def loadTypes(config: Config): Map[String, FieldType] = {
    entries(config, "dictionary.global.types").map { e =>
      val (k, v) = toPair(e)
      k -> FieldType.withName(v)
    }.toMap
  }

  def toPair(e: Entry[String, ConfigValue]): (String, String) = {
    val path: mutable.Buffer[String] = ConfigUtil.splitPath(e.getKey)
    val key: String = path.mkString(".")
    val value = e.getValue.unwrapped().toString
    key -> value
  }
}

object FieldType extends Enumeration {
  type FieldType = Value
  val String, Integer = Value
}