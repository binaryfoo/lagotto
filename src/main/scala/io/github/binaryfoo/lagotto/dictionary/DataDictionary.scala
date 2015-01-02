package io.github.binaryfoo.lagotto.dictionary
import io.github.binaryfoo.lagotto.LogLike
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType

trait DataDictionary {

  // falls back to short name
  def englishNameOf(field: String, context: LogLike): Option[String]

  def shortNameOf(field: String, context: LogLike): Option[String]

  /**
   * Always fall back to field.
   */
  final def exportNameOf(field: String, context: LogLike): String = {
    optionalExportNameOf(field, context).getOrElse(field)
  }

  /**
   * Always default to String.
   */
  final def typeOf(field: String, context: LogLike): FieldType = {
    optionalTypeOf(field, context).getOrElse(FieldType.String)
  }

  def optionalExportNameOf(field: String, context: LogLike): Option[String]

  def optionalTypeOf(field: String, context: LogLike): Option[FieldType]

  def translateValue(field: String, context: LogLike, value: String): Option[String]

  /**
   * Reverse lookup of the field path for the short name that applies based on the context.
   * Eg Given name = stan find 11 as the field.
   */
  def fieldForShortName(name: String, context: LogLike): Option[String]
}

object FieldType extends Enumeration {
  type FieldType = Value
  val String, Integer = Value
}
