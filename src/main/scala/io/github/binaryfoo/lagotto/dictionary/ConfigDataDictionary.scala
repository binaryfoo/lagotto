package io.github.binaryfoo.lagotto.dictionary

import java.util
import java.util.Map.Entry

import com.typesafe.config.{ConfigObject, Config, ConfigValue}
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.ConfigWrapper.{toPath,RichConfig}
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.mapAsScalaMap
import scala.collection.mutable

/**
 * Field number to human name translation.
 *
 * Debug using:
 *   -Dconfig.trace=loads
 */
case class ConfigDataDictionary(config: Config, name: String = "root") extends DataDictionary {

  val englishNames = loadEnglishNames(config)
  lazy val snakeNames = englishNames.mapValues(SnakeCase.toSnakeCase)
  lazy val camelNames = englishNames.mapValues(CamelCase.toCamelCase)
  val shortNames = loadShortNames(config)
  lazy val reverseShortNames = inverse(snakeNames) ++ inverse(camelNames) ++ inverse(shortNames)
  val types = loadTypes(config)
  val translations = loadTranslations(config)

  /*
  Needs:
    1. name fields
    2. name values in fields: MTI, NMIC
    3. override defaults based on realm, mti, nmic
   */

  override def nameOf(nameType: NameType, field: String, context: LogEntry): Option[String] = {
    val hit = nameType match {
      case NameType.English => englishNames.get(field).orElse(nameOf(NameType.Short, field, context))
      case NameType.Short => shortNames.get(field)
      case NameType.Snake => shortNames.get(field).orElse(snakeNames.get(field))
      case NameType.Camel => shortNames.get(field).orElse(camelNames.get(field))
    }
    if (hit.isDefined)
      Debug.log(s"nameOf type $nameType field $field = ${hit.get} from $name")
    hit
  }

  override def optionalTypeOf(field: String, context: LogEntry): Option[FieldType] = {
    val hit = types.get(field)
    if (hit.isDefined)
      Debug.log(s"typeOf field $field = ${hit.get} from $name")
    hit
  }

  override def translateValue(field: String, context: LogEntry, value: String): Option[String] = {
    val table = translations.collectFirst {
      case Translations(f, filter, t) if f == field && filter(context) => t
    }
    val hit = table.flatMap(_.get(value))
    if (hit.isDefined)
      Debug.log(s"translation of value $value in field $field = ${hit.get} from $name")
    hit
  }

  override def fieldForShortName(name: String, context: LogEntry): Option[String] = {
    val hit = reverseShortNames.get(name)
    if (hit.isDefined)
      Debug.log(s"field '$name' = ${hit.get} from ${this.name}")
    hit
  }

  override def toString: String = name

  private def loadEnglishNames(config: Config): Map[String, String] = {
    val fields = config.entries("fields")
    val subfields = config.entries("subfields")
    (fields ++ subfields).map(toPair).flatMap(FieldPath.expandPair).toMap
  }

  private def loadShortNames(config: Config): Map[String, String] = {
    config.entries("shortNames").map(toPair).toMap
  }

  private def loadTypes(config: Config): Map[String, FieldType] = {
    config.entries("types").map { e =>
      val (k, v) = toPair(e)
      k -> FieldType.forName(v).getOrElse(throw new IAmSorryDave(s"Unknown Field Type '$v' for value $k"))
    }.toMap
  }

  private def loadTranslations(config: Config): Seq[Translations] = {
    if (config.hasPath("translations")) {
      val all: mutable.Buffer[_ <: ConfigObject] = config.getObjectList("translations")
      all.map { c =>
        val field = c.get("field").unwrapped().asInstanceOf[String]
        val filter = Option(c.get("filter")).flatMap(v => LogFilters.NaiveParser.parseAndExpr(v.unwrapped().asInstanceOf[String])).getOrElse(AllFilter)
        val table: mutable.Map[String, String] = c.get("values").unwrapped().asInstanceOf[util.Map[String, String]]
        Translations(field, filter, table.toMap)
      }
    } else {
      Seq()
    }
  }

  def toPair(e: Entry[String, ConfigValue]): (String, String) = {
    val key = toPath(e.getKey)
    val value = e.getValue.unwrapped().toString
    key -> value
  }

  private def inverse(m: Map[String, String]) = m.map { case (k, v) => v -> k }
}

case class Translations(field: String, filter: LogFilter, table: Map[String, String])