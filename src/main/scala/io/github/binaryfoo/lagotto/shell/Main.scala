package io.github.binaryfoo.lagotto.shell

import com.typesafe.config.{Config, ConfigFactory}
import io.github.binaryfoo.lagotto.JoinMode.JoinMode
import io.github.binaryfoo.lagotto.MsgPair.RichEntryIterable
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.reader._

import scala.annotation.tailrec

object Main extends App {

  runWith(args, ConfigFactory.load())

  def runWith(args: Array[String], config: Config) = {
    new OptionsParser(config).parse(args).foreach { opts =>
      try {
        if (Debug.enabled)
          Console.err.println(opts)

        val (pipeline, format) = (new Pipeline(opts, config))()
        val sink = sinkFor(opts, format)

        pipeline.foreach(sink.entry)
        sink.finish()
        opts.progressMeter.finish()
      }
      catch {
        case e: Exception if !Debug.enabled && !e.isInstanceOf[NullPointerException] =>
          Console.err.println(ExceptionTrace.messageTrace(e))
      }
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
      val sinks = if (opts.liveUi)
        Seq(dataFile, gnuplotScript, new OnFinishWebServerSink(baseName + ".svg"))
      else
        Seq(dataFile, gnuplotScript)
      new CompositeSink(sinks)
    } else if (opts.liveUi) {
      new LiveWebServerSink(format)
    } else {
      new IncrementalSink(format, opts.header)
    }
  }
}

case class SortOrder(afterGrouping: Seq[SortKey] = Seq.empty, beforeGrouping: Seq[SortKey] = Seq.empty)

case class Filters(aggregate: Seq[LogFilter] = Seq(), delay: Seq[LogFilter] = Seq(), paired: Seq[LogFilter] = Seq())

class Pipeline(val opts: CmdLineOptions, val config: Config) {

  def apply(): (Iterator[LogEntry], OutputFormat) = {
    val SortOrder(postAggregationSortKey, preAggregationSortKey) = partitionSortKey()
    val filters = partitionFilters()

    val paired = if (opts.pair) read(JposLog).pair() else read(opts.inputFormat)
    val joined = join(paired, opts.joinOn, opts.inputFormat)
    val firstFilter = filter(joined, filters.paired)
    val sorted = sort(firstFilter, preAggregationSortKey)
    val withDelays = addDelays(sorted)
    val secondFilter = filter(withDelays, filters.delay)
    val aggregated = applyAggregation(secondFilter)
    val thirdFilter = filter(aggregated, filters.aggregate)
    val secondSort = sort(thirdFilter, postAggregationSortKey)
    val pivot = applyPivot(secondSort)
    val format = outputFormat(pivot)
    val limited = if (opts.limit.isDefined) pivot.take(opts.limit.get) else pivot
    (limited, format)
  }

  def partitionSortKey(): SortOrder = {
    val (afterGrouping, beforeGrouping) = opts.sortBy.partition {
      case SortKey(key@HasAggregateExpressions(_),_,_) => true
      case SortKey(DelayExpr,_,_) => true
      case k => false
    }
    SortOrder(afterGrouping, beforeGrouping)
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
    val reader = if (System.getProperty("single.thread") == "true") {
      SingleThreadLogReader(strict = opts.strict, progressMeter = opts.progressMeter, logType = logType)
    } else {
      LogReader(strict = opts.strict, progressMeter = opts.progressMeter, logType = logType)
    }
    val raw = reader.readFilesOrStdIn(opts.input.sortBy(LogFiles.sequenceNumber), opts.follow)
    if (opts.merge) raw.filter(new DeDuplicator) else raw
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

  private def sort(v: Iterator[LogEntry], sortBy: Seq[SortKey]): Iterator[LogEntry] = {
    if (sortBy.isEmpty) {
      v
    } else {
      trySort(v.toSeq, new SortKeyOrdering(sortBy.toList))
    }
  }

  // Screaming insanity to attempt a sort by integer comparison first then yet fall back to string ...
  // Options: could try to guess from they name of the key or write an Ordering[Any]?
  @tailrec
  private def trySort(memoryHog: Seq[LogEntry], key: SortKeyOrdering): Iterator[LogEntry] = {
    try {
      memoryHog.sorted(key).toIterator
    }
    catch {
      case e: Exception =>
        val indexToFallback = key.keys.indexWhere(_.asNumber)
        if (indexToFallback == -1) {
          throw new IAmSorryDave(s"Sort failed: $e")
        } else {
          val keyToFallback = key.keys(indexToFallback)
          val newKey = key.keys.updated(indexToFallback, keyToFallback.copy(asNumber = false))
          trySort(memoryHog, new SortKeyOrdering(newKey))
        }
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
      AggregateExpr.aggregate(v, aggregationConfig.keys, aggregationConfig.aggregates.toSeq)
    }
  }

  private def applyPivot(entries: Iterator[LogEntry]): Iterator[LogEntry] = {
    if (opts.pivot().isDefined) {
      val fields = opts.outputFields()
      val rotateOn = fields.collectFirst { case e: DirectExpr => e }.get
      val pivotExpr = opts.pivot().get
      val pivoted = fields.filterNot(f => f == rotateOn || f == pivotExpr)
      val materialized = entries.toList
      if (pivotExpr.distinctValues().isEmpty)
        materialized.foreach(pivotExpr)
      new PivotedIterator(rotateOn, pivotExpr, pivoted, materialized.iterator)
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
