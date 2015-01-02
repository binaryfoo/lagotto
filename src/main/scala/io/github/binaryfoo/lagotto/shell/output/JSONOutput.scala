package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.dictionary.{FieldType, DataDictionary}
import io.github.binaryfoo.lagotto.output.DeadSimpleJsonWriter
import io.github.binaryfoo.lagotto.shell.OutputFormat
import io.github.binaryfoo.lagotto.{ISO8601TimeFormat, LogLike}

object JSONOutput extends OutputFormat {

  override def header(): Option[String] = None

  // TODO unpack realm fields?

  override def apply(e: LogLike): Option[String] = {
    val writer = new DeadSimpleJsonWriter()

    val timestamp = e.timestamp
    if (timestamp != null)
      writer.add("at", ISO8601TimeFormat.print(timestamp))

    e.exportAsSeq.foreach { case (k, v) =>
      (DataDictionary.exportNameOf(k, e), DataDictionary.typeOf(k, e)) match {
        case ("at", _) => // ignore
        case (name, FieldType.Integer) => writer.addAsInt(name, v)
        case (name, FieldType.String) => writer.add(name, v)
      }
    }

    writer.done()

    Some(writer.toString)
  }

  override def footer(): Option[String] = None
}
