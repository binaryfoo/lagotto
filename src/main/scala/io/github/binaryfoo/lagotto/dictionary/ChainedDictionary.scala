package io.github.binaryfoo.lagotto.dictionary

import io.github.binaryfoo.lagotto.dictionary.NameType.NameType
import io.github.binaryfoo.lagotto.{AllFilter, LogFilter, LogLike}
import io.github.binaryfoo.lagotto.dictionary.FieldType.FieldType

case class ChainedDictionary(current: DataDictionary, filter: LogFilter = AllFilter, next: DataDictionary = ChainEnd) extends DataDictionary {
  
  override def nameOf(nameType: NameType, field: String, context: LogLike): Option[String] = {
    (if (filter(context)) current.nameOf(nameType, field, context)
     else None)
    .orElse(next.nameOf(nameType, field, context))
  }

  override def translateValue(field: String, context: LogLike, value: String): Option[String] = {
    (if (filter(context)) current.translateValue(field, context, value)
     else None)
    .orElse(next.translateValue(field, context, value))
  }

  override def fieldForShortName(name: String, context: LogLike): Option[String] = {
    (if (filter(context)) current.fieldForShortName(name, context)
     else None)
    .orElse(next.fieldForShortName(name, context))
  }

  override def optionalTypeOf(field: String, context: LogLike): Option[FieldType] = {
    (if (filter(context)) current.optionalTypeOf(field, context)
     else None)
    .orElse(next.optionalTypeOf(field, context))
  }
}

object ChainEnd extends DataDictionary {
  
  override def nameOf(nameType: NameType, field: String, context: LogLike): Option[String] = None

  override def translateValue(field: String, context: LogLike, value: String): Option[String] = None

  override def fieldForShortName(name: String, context: LogLike): Option[String] = None

  override def optionalTypeOf(field: String, context: LogLike): Option[FieldType] = None

}
