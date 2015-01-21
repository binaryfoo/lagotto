package io.github.binaryfoo.lagotto.dictionary

import java.io.{File, FilenameFilter}

import com.typesafe.config.{ConfigObject, Config, ConfigFactory, ConfigValue}
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.ConfigWrapper.RichConfig
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType

/**
 * Field number to human name translation.
 *
 * Debug using:
 *   -Dconfig.trace=loads
 */
case class RootDataDictionary(config: Config = ConfigFactory.load()) extends DataDictionary {

  val chain: DataDictionary = buildChain() 

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

  def buildChain(): DataDictionary = {
    customDictionaries.foldRight(defaultDictionary) { case ((filter, dictionary), tail) =>
      ChainedDictionary(dictionary, filter, tail)
    }
  }

  private def defaultDictionary = ChainedDictionary(ConfigDataDictionary(config.getConfig("dictionaries.global")))

  private def customDictionaries: Array[(LogFilter, DataDictionary)] = {
    val customDirectory = new File(config.getString("custom.dictionaries.dir"))
    val customDictionariesFromClasspath = config.getObjectOrDie("dictionaries").toSeq.filterNot { case (name, _) => name == "global"}
    customDictionaryFiles(customDirectory).flatMap { f =>
      val custom = ConfigFactory.parseFile(f)
      val dictionaries = custom.getObjectOrDie("dictionaries")
      load(dictionaries.toSeq)
    } ++ load(customDictionariesFromClasspath)
  }

  def load(dictionaries: Seq[(String, ConfigValue)]): Seq[(AndFilter, ConfigDataDictionary)] = {
    dictionaries.map { case (name, v) =>
      val dictionary = v.asInstanceOf[ConfigObject].toConfig
      val filterText = dictionary.getString("filter")
      val origin = dictionary.origin().description()
      val filter = LogFilters.NaiveParser.parseAndExpr(filterText).getOrElse(throw new IAmSorryDave(s"Failed to parse filter '$filterText' from $origin"))
      filter -> ConfigDataDictionary(dictionary, s"$name@$origin")
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