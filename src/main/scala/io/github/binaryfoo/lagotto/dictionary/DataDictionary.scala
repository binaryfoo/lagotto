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
}

object FieldType extends Enumeration {
  type FieldType = Value
  val String, Integer = Value
}
