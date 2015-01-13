package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.RootDataDictionary
import io.github.binaryfoo.lagotto.reader.{JposLog, LogType, AutoDetectLog, LogReader}

import scala.util.Try

object Main extends App {

  val dictionary = RootDataDictionary()
  FieldExpr.dictionary = Some(dictionary)

  Options.parse(args, dictionary).map { config =>
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

case class SortOrder(afterGrouping: Option[FieldExpr] = None, beforeGrouping: Option[FieldExpr] = None)

case class Filters(aggregate: Seq[LogFilter] = Seq(), delay: Seq[LogFilter] = Seq(), paired: Seq[LogFilter] = Seq())

class Pipeline(val config: Config) {

  def apply(): Iterator[LogLike] = {
    val SortOrder(postAggregationSortKey, preAggregationSortKey) = partitionSortKey()
    val filters = partitionFilters()

    val paired = if (config.pair) read(JposLog).pair() else read(AutoDetectLog)
    val firstFilter = filter(paired, filters.paired)
    val sorted = sort(firstFilter, preAggregationSortKey, config.sortDescending)
    val withDelays = addDelays(sorted)
    val secondFilter = filter(withDelays, filters.delay)
    val aggregated = applyAggregation(secondFilter)
    val thirdFilter = filter(aggregated, filters.aggregate)
    val secondSort = sort(thirdFilter, postAggregationSortKey, config.sortDescending)
    if (config.limit.isDefined) secondSort.take(config.limit.get)
    else secondSort
  }

  def partitionSortKey(): SortOrder = {
    config.sortBy.map {
      case key@HasAggregateExpressions(_) => SortOrder(afterGrouping = Some(key))
      case DelayExpr => SortOrder(afterGrouping = Some(DelayExpr))
      case k => SortOrder(beforeGrouping = Some(k))
    }.getOrElse(SortOrder())
  }
  
  def partitionFilters(): Filters = {
    val aggregate = config.filters.collect {
      case f@FieldFilterOn(HasAggregateExpressions(_)) => f
    }
    val delay = config.filters.collect {
      case f@FieldFilterOn(DelayExpr) => f
    }
    val paired = config.filters.diff(aggregate ++ delay)
    Filters(aggregate, delay, paired)
  }

  private def read[T <: LogLike](logType: LogType[T]): Iterator[T] = {
    val reader = LogReader(strict = config.strict, progressMeter = config.progressMeter, logType = logType)
    reader.readFilesOrStdIn(config.input.sortBy(LogFiles.sequenceNumber))
  }

  private def filter(v: Iterator[LogLike], filters: Seq[LogFilter]): Iterator[LogLike] = {
    val shouldInclude = AndFilter(filters)

    if (filters.isEmpty) {
      v
    } else if (config.beforeContext == 0 && config.afterContext == 0) {
      // premature optimization for this case?
      v.filter(shouldInclude)
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
      }
    }
  }

  private def sort(v: Iterator[LogLike], sortBy: Option[FieldExpr], descending: Boolean): Iterator[LogLike] = {
    if (sortBy.isEmpty) {
      v
    } else {
      val key = sortBy.get
      // Screaming insanity to attempt a sort by integer comparison first then yet fall back to string ...
      // Options: could try to guess from they name of the key or write an Ordering[Any]?
      val memoryHog = v.toSeq
      val sorted = Try(memoryHog.sortBy(key(_).toInt)).getOrElse(memoryHog.sortBy(key(_)))
      (if (descending) sorted.reverse else sorted).toIterator
    }
  }

  private def addDelays(v: Iterator[LogLike]): Iterator[LogLike] = {
    if (config.requiresDelayCalculation())
      DelayExpr.calculateDelays(v)
    else
      v
  }

  private def applyAggregation(v: Iterator[LogLike]): Iterator[LogLike] = {
    val aggregationConfig = config.aggregationConfig()
    if (aggregationConfig.aggregates.isEmpty) {
      v
    } else {
      AggregateExpr.aggregate(v, aggregationConfig.keys, aggregationConfig.aggregates.toSeq).toIterator
    }
  }
}
