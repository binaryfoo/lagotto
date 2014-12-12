package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable
import io.github.binaryfoo.isotools.shell.FieldFilter.MatchOp
import io.github.binaryfoo.isotools.{ConvertibleToMap, LogReader}
import scopt.Read

object Main extends App {

  Options.parse(args).map { config =>
    def logEntries = LogReader(config.strict).readFilesOrStdIn(config.input)

    def pipeline: Stream[ConvertibleToMap] = {
      var entries: Stream[ConvertibleToMap] = if (config.pair)
        logEntries.pair()
      else
        logEntries

      if (config.header) {
        config.format.header().map(println(_))
      }

      if (config.filters.nonEmpty) {
        entries = entries.applyFilters(config.filters, config.beforeContext, config.afterContext)
      }

      // presumably both these options prevent incremental output
      if (config.sortBy != null) {
        entries = entries.sortBy(_(config.sortBy))
        if (config.sortDescending) {
          entries = entries.reverse
        }
      }
      entries
    }

    pipeline
      .map(e => config.format(e))
      .foreach(e => println(e))
  }

  implicit class RichLogEntries(val v: Stream[ConvertibleToMap]) extends AnyVal {

    def applyFilters(filters: Seq[LogFilter], beforeContext: Int, afterContext: Int): Stream[ConvertibleToMap] = {
      val shouldInclude = (m: ConvertibleToMap) => filters.forall(_(m))
      if (beforeContext == 0 && afterContext == 0) {
        // premature optimization for this case?
        v.filter(shouldInclude)
      } else {
        val preceding = new BoundedQueue[ConvertibleToMap](beforeContext)
        var aftersNeeded = 0
        v.flatMap { item =>
          if (shouldInclude(item)) {
            aftersNeeded = afterContext
            preceding.dump() :+ item
          } else if (aftersNeeded > 0) {
            aftersNeeded -= 1
            List(item)
          } else {
            preceding.add(item)
            List.empty
          }
        }
      }
    }
  }

}
