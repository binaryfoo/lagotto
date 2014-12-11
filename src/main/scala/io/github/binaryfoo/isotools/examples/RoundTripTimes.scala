package io.github.binaryfoo.isotools.examples

import io.github.binaryfoo.isotools.LogReader
import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable

object RoundTripTimes extends App {

  LogReader().readFilesOrStdIn(args).pair()
    .filter(_.mti == "0200")
    .foreach(p => println(p.toCsv("time", "mti", "11", "rtt")))
}
