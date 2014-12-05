package io.github.binaryfoo.isotools.examples

import java.io.File

import io.github.binaryfoo.isotools.LogReader
import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable
import io.github.binaryfoo.isotools.Csv.MapToCsv

import scala.io.BufferedSource

object RoundTripTimes extends App {

  val entries = if (args.isEmpty)
    LogReader.read(new BufferedSource(System.in))
  else
    LogReader.read(args.map(new File(_)))

  entries.pair()
    .filter(_.mti == "0200")
    .foreach(p => println(p.toCsv("time", "mti", "11", "rtt")))
}
