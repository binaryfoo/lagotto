package io.github.binaryfoo.lagotto.dictionary

import java.io.{File, FilenameFilter}

import com.typesafe.config.{ConfigObject, Config, ConfigFactory, ConfigValue}
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.ConfigWrapper.RichConfig
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType

import scala.collection.JavaConversions
import scala.collection.mutable.ArrayBuffer

/**
 * Field number to human name translation.
 *
 * Debug using:
 *   -Dconfig.trace=loads
 */
case class RootDataDictionary(config: Config = ConfigFactory.load()) extends DataDictionary {

  val chain: ChainedDictionary = buildChain()

  override def nameOf(nameType: NameType, field: String, context: LogEntry): Option[String] = {
    chain.nameOf(nameType, field, context)
  }

  override def optionalTypeOf(field: String, context: LogEntry): Option[FieldType] = {
    chain.optionalTypeOf(field, context)
  }

  override def translateValue(field: String, context: LogEntry, value: String): Option[String] = {
    chain.translateValue(field, context, value)
  }

  override def fieldForShortName(name: String, context: LogEntry): Option[String] = {
    chain.fieldForShortName(name, context)
  }

  def possibleFieldsForShortName(name: String): Option[ShortNameLookup] = {
    var current: DataDictionary = chain
    val mappings = new ArrayBuffer[(LogFilter, String)]()
    while (current != null) {
      current match {
        case ChainedDictionary(d, filter, next) =>
          val field = d.fieldForShortName(name, null)
          if (field.isDefined) {
            mappings += ((filter, field.get))
          }
          current = next
        case _ =>
          current = null
      }
    }
    if (mappings.nonEmpty)
      Some(new ShortNameLookup(mappings))
    else
      None
  }

  def nameChain(): Seq[String] = chain.nameChain()

  private def buildChain(): ChainedDictionary = {
    val dictionariesFromClasspath = JavaConversions.asScalaBuffer(config.getList("dictionaries"))
    val defaultDictionary = ChainedDictionary(ConfigDataDictionary(dictionariesFromClasspath.head.asInstanceOf[ConfigObject].toConfig))
    val customDirectory = new File(config.getString("custom.dictionaries.dir"))
    val customDictionaries = customDictionaryFiles(customDirectory).flatMap { f =>
      val custom = ConfigFactory.parseFile(f).resolve()
      val dictionaries = JavaConversions.asScalaBuffer(custom.getList("dictionaries"))
      load(dictionaries)
    } ++ load(dictionariesFromClasspath.drop(1))

    customDictionaries.foldRight(defaultDictionary) { case ((filter, dictionary), tail) =>
      ChainedDictionary(dictionary, filter, tail)
    }
  }

  private def load(dictionaries: Seq[ConfigValue]): Seq[(LogFilter, ConfigDataDictionary)] = {
    dictionaries.map { case v =>
      val dictionary = v.asInstanceOf[ConfigObject].toConfig
      val name = dictionary.getString("name")
      val origin = dictionary.origin().description()
      val filter = filterFor(dictionary, origin)
      filter -> ConfigDataDictionary(dictionary, s"$name@$origin")
    }
  }

  private def filterFor(dictionary: Config, origin: String): LogFilter = {
    if (dictionary.hasPath("filter")) {
      val filterText = dictionary.getString("filter")
      LogFilters.NaiveParser.parseAndExpr(filterText).getOrElse(throw new IAmSorryDave(s"Failed to parse filter '$filterText' from $origin"))
    } else {
      AllFilter
    }
  }

  private def customDictionaryFiles(dir: File): Array[File] = {
    if (dir.exists()) {
      dir.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(".conf")
      })
    } else {
      Array.empty
    }
  }
}

/**
 * A subset of dictionary mappings used for a single short name.
 * We still need a set of (LogFilter, field name) pairs for the short name since a given short name can map to multiple
 * different fields depending on the log entry. Typically based mti and optionally nmic and/or realm.
 */
class ShortNameLookup(mappings: Seq[(LogFilter, String)]) {
  def valueForShortName(e: LogEntry): String = {
    for ((filter, field) <- mappings) {
      if (filter(e)) {
        return e(field)
      }
    }
    null
  }
}