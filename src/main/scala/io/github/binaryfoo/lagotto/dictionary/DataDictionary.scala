package io.github.binaryfoo.lagotto.dictionary
import io.github.binaryfoo.lagotto.LogLike
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType

trait DataDictionary {

  /**
   * Intent: Full wordy english to help somebody understand on first encounter.
   */
  final def englishNameOf(field: String, context: LogLike): Option[String] = {
    nameOf(NameType.English, field, context)
  }

  /**
   * Intent: you know the meaning of the term you just can't remember the name for the number.
   *
   * Couple of letters for filters, field lists and export.
   */
  def shortNameOf(field: String, context: LogLike): Option[String] = {
    nameOf(NameType.Short, field, context)
  }

  /**
   * Intent: short name, then camel cased english name, finally just fall back to field.
   */
  final def exportNameOf(field: String, context: LogLike): String = {
    nameOf(NameType.Export, field, context)
      .getOrElse(field)
  }

  def nameOf(nameType: NameType, field: String, context: LogLike): Option[String]

  /**
   * Always default to String.
   */
  final def typeOf(field: String, context: LogLike): FieldType = {
    optionalTypeOf(field, context).getOrElse(FieldType.String)
  }

  def optionalTypeOf(field: String, context: LogLike): Option[FieldType]

  def translateValue(field: String, context: LogLike, value: String): Option[String]

  /**
   * Reverse lookup of the field path for the short name that applies based on the context.
   * Eg Given name = stan find 11 as the field.
   */
  def fieldForShortName(name: String, context: LogLike): Option[String]
}

object NameType extends Enumeration {
  type NameType = Value
  val English, Short, Export = Value
}

object FieldType extends Enumeration {
  type FieldType = Value
  val String, Integer, GZippedString = Value

  def forName(s: String): Option[Value] = values.find(_.toString == s)
}
