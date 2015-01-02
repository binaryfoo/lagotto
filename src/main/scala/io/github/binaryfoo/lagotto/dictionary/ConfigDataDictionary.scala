package io.github.binaryfoo.lagotto.dictionary

import java.util.Map.Entry

import com.typesafe.config.{Config, ConfigValue}
import io.github.binaryfoo.lagotto.LogLike
import io.github.binaryfoo.lagotto.dictionary.ConfigWrapper.{toPath,RichConfig}
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType

/**
 * Field number to human name translation.
 *
 * Debug using:
 *   -Dconfig.trace=loads
 */
case class ConfigDataDictionary(config: Config, name: String = "root") extends DataDictionary {

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
  override def englishNameOf(field: String, context: LogLike): Option[String] = {
    englishNames.get(field).orElse(shortNameOf(field, context))
  }

  override def shortNameOf(field: String, context: LogLike): Option[String] = {
    shortNames.get(field)
  }

  // short name, field name camel cased, or just field
  override def optionalExportNameOf(field: String, context: LogLike): Option[String] = {
    shortNames.get(field)
      .orElse(exportNames.get(field))
  }

  override def optionalTypeOf(field: String, context: LogLike): Option[FieldType] = {
    types.get(field)
  }

  private def loadEnglishNames(config: Config): Map[String, String] = {
    val fields = config.entries("fields")
    val subfields = config.entries("subfields")
    (fields ++ subfields).map(toPair).toMap
  }

  private def loadShortNames(config: Config): Map[String, String] = {
    config.entries("shortNames").map(toPair).toMap
  }

  private def loadTypes(config: Config): Map[String, FieldType] = {
    config.entries("types").map { e =>
      val (k, v) = toPair(e)
      k -> FieldType.withName(v)
    }.toMap
  }

  def toPair(e: Entry[String, ConfigValue]): (String, String) = {
    val key = toPath(e.getKey)
    val value = e.getValue.unwrapped().toString
    key -> value
  }

}