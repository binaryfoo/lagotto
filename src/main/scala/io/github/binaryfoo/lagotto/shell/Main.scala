package io.github.binaryfoo.lagotto.shell

import com.typesafe.config.{Config, ConfigFactory}
import io.github.binaryfoo.lagotto.JoinMode.JoinMode
import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.RootDataDictionary
import io.github.binaryfoo.lagotto.reader._

import scala.util.Try

object Main extends App {

  runWith(args, ConfigFactory.load())

  def runWith(args: Array[String], config: Config) = {
    val dictionary = RootDataDictionary(config)

    new OptionsParser(dictionary).parse(args).map { opts =>
      val (pipeline, format) = (new Pipeline(opts, config))()
      val sink = sinkFor(opts, format)

      pipeline.foreach(sink.entry)
      sink.finish()
      opts.progressMeter.finish()
    }
  }

  def sinkFor(opts: CmdLineOptions, format: OutputFormat) = {
    if (opts.histogramFields.size == 1) {
      new SingleHistogramSink(opts.histogramFields.head)
    } else if (opts.histogramFields.size > 1) {
      val fields = opts.histogramFields.toList
      new MultipleHistogramSink(fields.dropRight(1), fields.last)
    } else if (opts.gnuplotFileName.isDefined) {
      val baseName = opts.gnuplotFileName.get
      val csvFileName = baseName + ".csv"
      val gpFileName = baseName + ".gp"
      val fields = OutputFormat.fieldsFor(format)
      val dataFile = new FileSink(new Tabular(fields, DelimitedTableFormat(",")), true, csvFileName)
      val gnuplotScript = new GnuplotSink(fields, csvFileName, gpFileName, baseName)
      new CompositeSink(Seq(dataFile, gnuplotScript))
    } else {
      new IncrementalSink(format, opts.header)
    }
  }
}

case class SortOrder(afterGrouping: Option[FieldExpr] = None, beforeGrouping: Option[FieldExpr] = None)

case class Filters(aggregate: Seq[LogFilter] = Seq(), delay: Seq[LogFilter] = Seq(), paired: Seq[LogFilter] = Seq())

class Pipeline(val opts: CmdLineOptions, val config: Config) {

  def apply(): (Iterator[LogEntry], OutputFormat) = {
    val SortOrder(postAggregationSortKey, preAggregationSortKey) = partitionSortKey()
    val filters = partitionFilters()
    val inputFormat = LogTypes.lookup(config, opts.inputFormat)

    val paired = if (opts.pair) read(JposLog).pair() else read(inputFormat)
    val joined = join(paired, opts.joinOn, inputFormat)
    val firstFilter = filter(joined, filters.paired)
    val sorted = sort(firstFilter, preAggregationSortKey, opts.sortDescending)
    val withDelays = addDelays(sorted)
    val secondFilter = filter(withDelays, filters.delay)
    val aggregated = applyAggregation(secondFilter)
    val thirdFilter = filter(aggregated, filters.aggregate)
    val secondSort = sort(thirdFilter, postAggregationSortKey, opts.sortDescending)
    val pivot = applyPivot(secondSort)
    val format = outputFormat(pivot)
    val limited = if (opts.limit.isDefined) pivot.take(opts.limit.get) else pivot
    (limited, format)
  }

  def partitionSortKey(): SortOrder = {
    opts.sortBy.map {
      case key@HasAggregateExpressions(_) => SortOrder(afterGrouping = Some(key))
      case DelayExpr => SortOrder(afterGrouping = Some(DelayExpr))
      case k => SortOrder(beforeGrouping = Some(k))
    }.getOrElse(SortOrder())
  }
  
  def partitionFilters(): Filters = {
    val aggregate = opts.filters.collect {
      case f@FieldFilterOn(HasAggregateExpressions(_)) => f
    }
    val delay = opts.filters.collect {
      case f@FieldFilterOn(DelayExpr) => f
    }
    val paired = opts.filters.diff(aggregate ++ delay)
    Filters(aggregate, delay, paired)
  }

  private def read[T <: LogEntry](logType: LogType[T]): Iterator[T] = {
    val reader = LogReader(strict = opts.strict, progressMeter = opts.progressMeter, logType = logType)
    reader.readFilesOrStdIn(opts.input.sortBy(LogFiles.sequenceNumber))
  }

  private def join(v: Iterator[LogEntry], join: Option[(FieldExpr, JoinMode)], logType: LogType[LogEntry]): Iterator[LogEntry] = {
    val delimiter = logType match {
      case xsv: XsvLog => xsv.delimiter
      case _ => '\n'
    }
    join.map {
      case (expr, JoinMode.Outer) => new Joiner(expr, delimiter).outerJoin(v)
      case (expr, JoinMode.Inner) => new Joiner(expr, delimiter).innerJoin(v)
    }.getOrElse(v)
  }

  private def filter(v: Iterator[LogEntry], filters: Seq[LogFilter]): Iterator[LogEntry] = {
    val shouldInclude = AndFilter(filters)

    if (filters.isEmpty) {
      v
    } else if (opts.beforeContext == 0 && opts.afterContext == 0) {
      // premature optimization for this case?
      v.filter(shouldInclude)
    } else {
      val preceding = new BoundedQueue[LogEntry](opts.beforeContext)
      var aftersNeeded = 0
      v.flatMap { item =>
        if (shouldInclude(item)) {
          aftersNeeded = opts.afterContext
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

  private def sort(v: Iterator[LogEntry], sortBy: Option[FieldExpr], descending: Boolean): Iterator[LogEntry] = {
    if (sortBy.isEmpty) {
      v
    } else {
      val key = sortBy.get
      // Screaming insanity to attempt a sort by integer comparison first then yet fall back to string ...
      // Options: could try to guess from they name of the key or write an Ordering[Any]?
      val memoryHog = v.toSeq
      val sorted = Try(memoryHog.sortBy(key(_).deNull("0").toInt)).getOrElse(memoryHog.sortBy(key(_).deNull()))
      (if (descending) sorted.reverse else sorted).toIterator
    }
  }

  private def addDelays(v: Iterator[LogEntry]): Iterator[LogEntry] = {
    if (opts.requiresDelayCalculation())
      DelayExpr.calculateDelays(v)
    else
      v
  }

  private def applyAggregation(v: Iterator[LogEntry]): Iterator[LogEntry] = {
    val aggregationConfig = opts.aggregationConfig()
    if (aggregationConfig.aggregates.isEmpty) {
      v
    } else {
      AggregateExpr.aggregate(v, aggregationConfig.keys, aggregationConfig.aggregates.toSeq).toIterator
    }
  }

  private def applyPivot(entries: Iterator[LogEntry]): Iterator[LogEntry] = {
    if (opts.pivot().isDefined) {
      val fields = opts.outputFields()
      val rotateOn = fields.collectFirst { case e: DirectExpr => e}
      val aggregates = opts.aggregationConfig().aggregates
      new PivotedIterator(rotateOn.get, opts.pivot().get, aggregates.toSeq, entries)
    } else {
      entries
    }
  }

  private def outputFormat(it: Iterator[LogEntry]): OutputFormat = {
    (it, opts.format) match {
      case (pivoted: PivotedIterator, t@Tabular(fields, f)) => t.copy(fields = pivoted.fields.map(PrimitiveExpr))
      case _ => opts.format
    }
  }

}
