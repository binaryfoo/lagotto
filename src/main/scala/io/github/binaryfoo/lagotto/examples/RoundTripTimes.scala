package io.github.binaryfoo.lagotto.examples

import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable
import io.github.binaryfoo.lagotto.reader.LogReader

object RoundTripTimes extends App {

  LogReader().readFilesOrStdIn(args).pair()
    .filter(_.mti == "0200")
    .foreach(p => println(p.toCsv("time", "mti", "11", "rtt")))
}
