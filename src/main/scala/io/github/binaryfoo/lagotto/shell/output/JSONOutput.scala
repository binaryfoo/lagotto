package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.output.DeadSimpleJsonWriter
import io.github.binaryfoo.lagotto.shell.OutputFormat
import io.github.binaryfoo.lagotto.{ISO8601TimeFormat, LogLike}

object JSONOutput extends OutputFormat {

  // full list from http://en.wikipedia.org/wiki/ISO_8583?
  private val dictionary = Map[String, String](
    "0" -> "mti",
    "4" -> "txnAmount",
    "5" -> "settlementAmount",
    "5" -> "cardholderBillingAmount",
    "11" -> "stan",
    "70" -> "nmic"
  ).withDefault(s => s)

  private val integerFields = Set[String]("4", "11", "lifespan")

  override def header(): Option[String] = None

  // TODO unpack realm fields?

  override def apply(e: LogLike): Option[String] = {
    val writer = new DeadSimpleJsonWriter()

    val timestamp = e.timestamp
    if (timestamp != null)
      writer.add("at", ISO8601TimeFormat.print(timestamp))

    e.toMap.foreach { case (k, v) =>
      k match {
        case "at" => // ignore
        case _ if integerFields.contains(k) => writer.addAsInt(dictionary(k), v)
        case _ => writer.add(dictionary(k), v)
      }
    }

    writer.done()

    Some(writer.toString)
  }

  override def footer(): Option[String] = None
}
