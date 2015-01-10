package io.github.binaryfoo.lagotto.dictionary

import java.io.{File, FilenameFilter}

import com.typesafe.config.{Config, ConfigFactory}
import io.github.binaryfoo.lagotto.dictionary.ConfigWrapper.RichConfig
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType
import io.github.binaryfoo.lagotto.{AndFilter, IAmSorryDave, LogFilter, LogLike}

import scala.collection.JavaConversions.asScalaSet

/**
 * Field number to human name translation.
 *
 * Debug using:
 *   -Dconfig.trace=loads
 */
case class RootDataDictionary(config: Config = ConfigFactory.load()) extends DataDictionary {

  val chain: DataDictionary = buildChain() 

  override def nameOf(nameType: NameType, field: String, context: LogLike): Option[String] = {
    chain.nameOf(nameType, field, context)
  }

  override def optionalTypeOf(field: String, context: LogLike): Option[FieldType] = {
    chain.optionalTypeOf(field, context)
  }

  override def translateValue(field: String, context: LogLike, value: String): Option[String] = {
    chain.translateValue(field, context, value)
  }

  override def fieldForShortName(name: String, context: LogLike): Option[String] = {
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
    customDictionaryFiles(customDirectory).flatMap { f =>
      val custom = ConfigFactory.parseFile(f)
      custom.getObjectOrDie("dictionaries").entrySet().map { e =>
        val name = e.getKey
        val dictionary = custom.getConfig(s"dictionaries.$name")
        val filterText = dictionary.getString("filter")
        val filter = AndFilter.unapply(filterText).getOrElse(throw new IAmSorryDave(s"Failed to parse filter '$filterText' from ${f.getName}"))
        filter -> ConfigDataDictionary(dictionary, name + "@" + f.getName)
      }
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