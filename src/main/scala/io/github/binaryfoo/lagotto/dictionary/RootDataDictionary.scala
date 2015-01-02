package io.github.binaryfoo.lagotto.dictionary

import java.io.{File, FilenameFilter}

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType
import io.github.binaryfoo.lagotto.{LogFilter, LogLike}

/**
 * Field number to human name translation.
 *
 * Debug using:
 *   -Dconfig.trace=loads
 */
case class RootDataDictionary(customDirectory: File = new File(System.getProperty("user.home", ""), ".lago/")) extends DataDictionary {

  val default: DataDictionary = ConfigDataDictionary(ConfigFactory.load().getConfig("dictionary.global"))
  val custom: Map[LogFilter, DataDictionary] = loadCustomDictionaries(customDirectory)

  // falls back to short name
  override def englishNameOf(field: String, context: LogLike): Option[String] = {
    customFor(context)
      .flatMap(_.englishNameOf(field, context))
      .orElse(default.englishNameOf(field, context))
  }

  override def shortNameOf(field: String, context: LogLike): Option[String] = {
    customFor(context)
      .flatMap(_.shortNameOf(field, context))
      .orElse(default.shortNameOf(field, context))
  }

  // short name, field name camel cased, or just field
  override def optionalExportNameOf(field: String, context: LogLike): Option[String] = {
    customFor(context)
      .flatMap(_.optionalExportNameOf(field, context))
      .orElse(default.optionalExportNameOf(field, context))
  }

  override def optionalTypeOf(field: String, context: LogLike): Option[FieldType] = {
    customFor(context)
      .flatMap(_.optionalTypeOf(field, context))
      .orElse(default.optionalTypeOf(field, context))
  }

  def customFor(context: LogLike): Option[DataDictionary] = custom.collectFirst {
    case (filter, dictionary) if filter(context) => dictionary
  }

  def loadCustomDictionaries(directory: File): Map[LogFilter, DataDictionary] = {
    customDictionaryFiles(directory).map { f =>
      val custom = ConfigFactory.parseFile(f)
      val filter = LogFilter.unapply(custom.getString("filter")).get
      filter -> ConfigDataDictionary(custom, f.getName)
    }.toMap
  }

  def customDictionaryFiles(dir: File): Array[File] = {
    if (dir.exists()) {
      dir.listFiles(new FilenameFilter {
        override def accept(dir: File, name: String): Boolean = name.endsWith(".conf")
      })
    } else {
      Array.empty
    }
  }
}