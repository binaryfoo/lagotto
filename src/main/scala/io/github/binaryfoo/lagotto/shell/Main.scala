package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable
import io.github.binaryfoo.lagotto._

import scala.util.Try

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
    } else if (config.gnuplotFileName.isDefined) {
      val baseName = config.gnuplotFileName.get
      val csvFileName = baseName + ".csv"
      val gpFileName = baseName + ".gp"
      val fields = config.format match {
        case Tabular(f, _) => f
      }
      val dataFile = new FileSink(new Tabular(fields, DelimitedTableFormat(",")), true, csvFileName)
      val gnuplotScript = new GnuplotSink(fields, csvFileName, gpFileName, baseName)
      new CompositeSink(Seq(dataFile, gnuplotScript))
    } else {
      new IncrementalSink(config.format, config.header)
    }
  }
}

case class SortOrder(afterGrouping: Option[GroundedFieldExpr] = None, beforeGrouping: Option[GroundedFieldExpr] = None)

case class Filters(aggregate: Seq[LogFilter] = Seq(), delay: Seq[LogFilter] = Seq(), paired: Seq[LogFilter] = Seq())

class Pipeline(val config: Config) {

  def apply(): Stream[LogLike] = {
    val SortOrder(postAggregationSortKey, preAggregationSortKey) = partitionSortKey()
    val filters = partitionFilters()
    // Need to understand if this insane nesting can be removed without introducing a memory leak.
    // ie pinning the whole Stream in memory.
    // I suspect not. Maybe Stream ain't a good idea.
    sort(
      filter(
        applyAggregation(
          filter(
            addDelays(
              sort(
                filter(
                  pair(read()).toIterator,
                  filters.paired),
                preAggregationSortKey, config.sortDescending)
            ).toIterator,
            filters.delay).toIterator).toIterator,
        filters.aggregate
      ),
      postAggregationSortKey, config.sortDescending)
  }

  def partitionSortKey(): SortOrder = {
    config.sortBy.map {
      case key@AggregateFieldExpr(_,_) => SortOrder(afterGrouping = Some(key))
      case key@SubtractTimeExpr(_, AggregateFieldExpr(_,_), AggregateFieldExpr(_,_)) => SortOrder(afterGrouping = Some(key))
      case DelayFieldExpr => SortOrder(afterGrouping = Some(DelayFieldExpr))
      case k => SortOrder(beforeGrouping = Some(k))
    }.getOrElse(SortOrder())
  }
  
  def partitionFilters(): Filters = {
    val aggregate = config.filters.collect {
      case f@FieldFilterOn(AggregateFieldExpr(_,_)) => f
      case f@FieldFilterOn(SubtractTimeExpr(_, AggregateFieldExpr(_,_), AggregateFieldExpr(_,_))) => f
    }
    val delay = config.filters.collect {
      case f@FieldFilterOn(DelayFieldExpr) => f
    }
    val paired = config.filters.diff(aggregate ++ delay)
    Filters(aggregate, delay, paired)
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
  private def sort(v: Stream[LogLike], sortBy: Option[GroundedFieldExpr], descending: Boolean): Stream[LogLike] = {
    if (sortBy.isEmpty) {
      v
    } else {
      val key = sortBy.get
      // Screaming insanity to attempt a sort by integer comparison first then yet fall back to string ...
      // Options: could try to guess from they name of the key or write an Ordering[Any]?
      if (descending) {
        Try(v.sortBy(key(_).toInt).reverse).getOrElse(v.sortBy(key(_)).reverse)
      } else {
        Try(v.sortBy(key(_).toInt)).getOrElse(v.sortBy(key(_)))
      }
    }
  }

  private def addDelays(v: Stream[LogLike]): Stream[LogLike] = {
    config.format match {
      case Tabular(fields, _) if fields.contains(DelayFieldExpr) => DelayTimer.calculateDelays(v)
      case _ => v
    }
  }

  // Iterator instead of Stream to the same reason as filter()
  private def applyAggregation(v: Iterator[LogLike]): Stream[LogLike] = {
    config.format match {
      case Tabular(fields, _) => AggregateLogLike.aggregate(v, fields)
      case _ => v.toStream
    }
  }
}
