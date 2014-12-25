package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable
import io.github.binaryfoo.lagotto._

object Main extends App {

  Options.parse(args).map { config =>
    val sink = sinkFor(config)
    val pipeline = new Pipeline(config)

    sink.start()
    pipeline().foreach(sink.entry)
    sink.finish()
    config.progressMeter.finish()
  }

  def sinkFor(config: Config) = {
    if (config.histogramFields.size == 1) {
      new SingleHistogramSink(config.histogramFields.head)
    } else if (config.histogramFields.size > 1) {
      val fields = config.histogramFields.toList
      new MultipleHistogramSink(fields.dropRight(1), fields.last)
    } else {
      new IncrementalSink(config.format, config.header)
    }
  }
}

class Pipeline(val config: Config) {

  def apply(): Stream[LogLike] = {
    // need to understand if this insane nesting can be removed without introducing a memory leak by pinning the whole
    // Stream in memory
    applyAggregation(
      filter(
        addDelays(
          sort(
            filter(pair(read()).toIterator,
              config.filters))).toIterator,
        config.secondStageFilters))
  }

  private def read() = {
    val reader = LogReader(strict = config.strict, progressMeter = config.progressMeter)
    reader.readFilesOrStdIn(config.input.sortBy(LogFiles.sequenceNumber))
  }

  private def pair(v: Stream[LogEntry]): Stream[LogLike] = if (config.pair) v.pair() else v

  // need an Iterator instead of Stream to prevent a call to filter() or flatMap() pinning the whole stream
  // in memory until the first match (if any)
  private def filter(v: Iterator[LogLike], filters: Seq[LogFilter]): Stream[LogLike] = {
    val shouldInclude = (m: LogLike) => filters.forall(_(m))

    if (filters.isEmpty) {
      v.toStream
    } else if (config.beforeContext == 0 && config.afterContext == 0) {
      // premature optimization for this case?
      v.filter(shouldInclude).toStream
    } else {
      val preceding = new BoundedQueue[LogLike](config.beforeContext)
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
      }.toStream
    }
  }

  // not always going to work in a bounded amount of memory
  private def sort(v: Stream[LogLike]): Stream[LogLike] = {
    if (config.sortBy == null) {
      v
    } else if (config.sortDescending) {
      v.sortBy(_(config.sortBy)).reverse
    } else {
      v.sortBy(_(config.sortBy))
    }
  }

  private def addDelays(v: Stream[LogLike]): Stream[LogLike] = {
    config.format match {
      case Tabular(fields, _) if fields.contains("delay") => DelayTimer.calculateDelays(v)
      case _ => v
    }
  }

  private def applyAggregation(v: Stream[LogLike]): Stream[LogLike] = {
    config.format match {
      case Tabular(fields, _) => AggregateLogLike.aggregate(v, fields)
      case _ => v
    }
  }
}
