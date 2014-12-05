package io.github.binaryfoo.isotools.examples

import io.github.binaryfoo.isotools.{Group, MsgPair, LogReader}
import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable
import io.github.binaryfoo.isotools.MsgPair.RichMsgPairIterable

object CoalesceOnKeySet extends App {

  LogReader.readFilesOrStdIn(args).pair()
    .filter(_.mti == "0200")
    .coalesce(e => {
      val normalized39 = if (e("39") == "01") e("39") else "ok"
      val keyset = e("53")
      s"KS $keyset with $normalized39"
    })
    .map({
      case p @ MsgPair(_,_) => p.toCsv("request.time", "0", "53", "39", "11")
      case Group(size, key) => s"... $size with $key ..."
    })
    .foreach(println(_))
}
