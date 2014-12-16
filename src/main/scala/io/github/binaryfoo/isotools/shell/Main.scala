package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable
import io.github.binaryfoo.isotools.shell.FieldFilter.MatchOp
import io.github.binaryfoo.isotools.{LogEntry, ConvertibleToMap, LogReader}
import scopt.Read

import scala.io.Source

object Main extends App {

  Options.parse(args).map { config =>
    if (config.header) {
      config.format.header().map(println(_))
    }
    val pipeline = new Pipeline(config)
    pipeline()
      .map(e => config.format(e))
      .foreach(e => println(e))
  }
}

class Pipeline(val config: Config) {
  
  def apply(): Stream[ConvertibleToMap] = {
    sort(filter(pair(read()).toIterator))
  }

  def read() = LogReader(config.strict).readFilesOrStdIn(config.input)

  def pair(v: Stream[LogEntry]): Stream[ConvertibleToMap] = if (config.pair) v.pair() else v

  // need an Iterator instead of Stream to prevent a call to filter() or flatMap() pinning the whole stream
  // in memory until the first match (if any)
  def filter(v: Iterator[ConvertibleToMap]): Iterator[ConvertibleToMap] = {
    val shouldInclude = (m: ConvertibleToMap) => config.filters.forall(_(m))

    if (config.filters.isEmpty) {
      v
    } else if (config.beforeContext == 0 && config.afterContext == 0) {
      // premature optimization for this case?
      v.filter(shouldInclude)
    } else {
      val preceding = new BoundedQueue[ConvertibleToMap](config.beforeContext)
      var aftersNeeded = 0
      v.flatMap { item =>
        if (shouldInclude(item)) {
          aftersNeeded = config.afterContext
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

  // not always going to work in a bounded amount of memory
  def sort(v: Iterator[ConvertibleToMap]): Stream[ConvertibleToMap] = {
    if (config.sortBy == null) {
      v.toStream
    } else if (config.sortDescending) {
      v.toStream.sortBy(_(config.sortBy)).reverse
    } else {
      v.toStream.sortBy(_(config.sortBy))
    }
  }
}
