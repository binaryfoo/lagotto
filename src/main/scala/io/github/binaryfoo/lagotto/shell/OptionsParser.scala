package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop

import com.typesafe.config.Config
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType
import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.highlight.{AnsiMarkup, NotMarkedUp}
import io.github.binaryfoo.lagotto.output.GnuplotFormat
import io.github.binaryfoo.lagotto.reader.{JposLog, LogType, LogTypes}
import io.github.binaryfoo.lagotto.shell.DelimitedTableFormat.{Csv, Tsv}
import io.github.binaryfoo.lagotto.shell.output._
import scopt.Read

class OptionsParser(val config: Config, val canHandleAnsi: Boolean = IsATty()) {

  private val dictionary = RootDataDictionary(config)
  private val logTypes = LogTypes.load(config)
  private val fieldParser = FieldExprParser(Some(dictionary))
  private val filterParser = new LogFilterParser(fieldParser)
  import fieldParser.FieldExpr
  import filterParser.LogFilter

  val tableFormats = Seq("tsv", "csv", "ascii", "html", "jira", "sqlIn")

  def parse(args: Array[String]): Option[CmdLineOptions] = {

    val defaultMarkup = if (canHandleAnsi) AnsiMarkup else NotMarkedUp
    val parser = new scopt.OptionParser[CmdLineOptions]("lago") {
      head(s"lagotto", "0.0.1")

      help("help") text "Show usage"

      arg[String]("<log-file>...") maxOccurs Int.MaxValue optional() action { (f, c) =>
        c.copy(input = c.input :+ f)
      } text "Optional list of log files. Otherwise read stdin."

      opt[LogType[LogEntry]]('i', "in-format") action { (fmt, c) =>
        c.copy(inputFormat = fmt, strict = defaultStrictFor(fmt))
      } text s"Optional input log file format. Supported: ${logTypes.keys.mkString(",")}"

      opt[String]('g', "grep") unbounded() action { (expr, c) =>
        c.copy(filters = c.filters :+ GrepFilter(expr))
      } text "Filter by messages including text"

      opt[String]("grep!") unbounded() action { (expr, c) =>
        c.copy(filters = c.filters :+ NegativeGrepFilter(expr))
      } text "Exclude messages containing text"

      opt[String]("igrep") unbounded() action { (expr, c) =>
        c.copy(filters = c.filters :+ InsensitiveGrepFilter(expr))
      } text "Case insensitive grep. Slower."

      opt[String]("igrep!") unbounded() action { (expr, c) =>
        c.copy(filters = c.filters :+ NegativeInsensitiveGrepFilter(expr))
      } text "Case insensitive grep!. Slower."

      opt[LogFilter]('f', "field") unbounded() action { case (filter, c) =>
          c.copy(filters = c.filters :+ filter)
      } keyValueName ("path", "value") text "Filter by field path. Eg 48.1.2=value. Operators: =, ~, >, < ~/regex/"

      opt[String]('t', "table") action { (fields, c) =>
        c.copy(table = c.table.copy(fields = fields))
      } text "Output a table of values instead of the raw logs"

      opt[TableFormatter]('o', "out-format") action { (formatter, c) =>
        val contentType = formatter match {
          case HtmlTableFormat => Html
          case _ => c.table.contentType
        }
        c.copy(table = c.table.copy(formatter = formatter, contentType = contentType))
      } text s"The output format for tables: ${tableFormats.mkString(", ")}"

      opt[Unit]("plain") action { (_, c) =>
        c.copy(table = c.table.copy(contentType = PlainText))
      } text "Don't use Unicode symbol characters that might not render"

      opt[Unit]("live") action { (_, c) =>
        c.copy(incremental = true)
      } text "Output table incrementally. Can be messy."

      opt[Unit]("json") action { (_, c) =>
        c.copy(format = JSONOutput(dictionary))
      } text "Output a line of JSON per log entry."

      opt[Unit]("digest") action { (_, c) =>
        c.copy(format = DigestedFormat(dictionary, Some(NameType.English), defaultMarkup))
      } text "Output full message in a compact format."

      opt[Unit]("names") action { (_, c) =>
        c.copy(format = NamedAndHighlightedFormat(dictionary, defaultMarkup))
      } text "XML with extra name=\"\" attribute for each <field/>"

      opt[Option[NameType]]("digest-as") action { (nameType, c) =>
        c.copy(format = DigestedFormat(dictionary, nameType, defaultMarkup))
      } text "Output full message in a compact format with name type: English, Short or Export."

      opt[String]("histogram") action { (fields, c) =>
        c.copy(histogramFields = parseFields(fields))
      } text "Output a histogram"

      opt[String]("influx") action { (measurement, c) =>
        val format = c.format match {
          case f@InfluxDBFormat(_, _, _) => f.copy(measurement = measurement)
          case _ => InfluxDBFormat(measurement = measurement)
        }
        c.copy(format = format)
      } text "Output influxdb points"

      opt[String]("tags") action { (tags, c) =>
        val format = c.format match {
          case f@InfluxDBFormat(_, _, _) => f.copy(tags = parseFields(tags))
          case _ => InfluxDBFormat(tags = parseFields(tags))
        }
        c.copy(format = format)
      } text "Tags for influxdb points"

      opt[String]("influx-fields") action { (fields, c) =>
        val format = c.format match {
          case f@InfluxDBFormat(_, _, _) => f.copy(fields = parseFields(fields))
          case _ => InfluxDBFormat(fields = parseFields(fields))
        }
        c.copy(format = format)
      } text "Fields for influxdb points"

      opt[String]("influx-url") action { (url, c) =>
        c.copy(influxDbUrl = Some(url))
      } text "URL to post influx points to"

      opt[String]("graphite") action { (url, c) =>
        c.copy(graphiteUrl = Some(url))
      } text "Graphite host:port to write metrics to. Use - for stdout."

      opt[String]("metric-prefix") action { (prefix, c) =>
        val politePrefix = if (prefix.endsWith(".")) prefix else prefix + "."
        c.copy(graphitePrefix = Some(politePrefix))
      } text "Optional prefix when outputting graphite metrics"

      opt[String]("graphite-events") action { (url, c) =>
        c.copy(graphiteEventUrl = Some(url))
      } text "Graphite host:port to write events to. Use - for stdout."

      opt[String]("event") action { (template, c) =>
        c.copy(graphiteEvent = Some(template))
      } text "Template to render in graphite event 'what' field."

      opt[Unit]("pair") action {(_, c) =>
        c.copy(pair = true)
      } text "Match requests with responses"

      opt[Unit]("no-header") action {(_, c) =>
        c.copy(header = false)
      } text "Don't print the table header row"

      opt[Int]('B', "before-context") action {(n,c) =>
        c.copy(beforeContext = n)
      } text "Like -B in grep"

      opt[Int]('A', "after-context") action {(n,c) =>
        c.copy(afterContext = n)
      } text "Like -A in grep"

      opt[Int]('C', "context") action {(n,c) =>
        c.copy(beforeContext = n, afterContext = n)
      } text "Like -C in grep"

      opt[String]("sort") action {(field,c) =>
        c.copy(sortBy = parseSortKey(field))
      } text "Sort output by field(s). Eg time,11 desc."

      opt[String]("join") action {(field,c) =>
        c.copy(joinOn = Some(FieldExpr.allOf(field), JoinMode.Outer))
      } text "Full outer join on the named field"

      opt[String]("inner-join") action {(field,c) =>
        c.copy(joinOn = Some(FieldExpr.allOf(field), JoinMode.Inner))
      } text "Show only paired rows (like SQL's default join)"

      opt[String]("gnuplot") action {(fileName,c) =>
        c.copy(gnuplot = c.gnuplot.copy(enabled = true, scriptName = fileName))
      } text "Write a gnuplot script <name>.gp. Write the output to <name>.csv. Only makes sense with --table"

      opt[String]("plot") action { (style, c) =>
        val (time, charts) = new PlotStatementParser(fieldParser).parse(style)
        c.copy(gnuplot = c.gnuplot.copy(timeField = time, charts = charts))
      } text "Number of plots"

      opt[String]("plot-time-format") action {(timeFormat,c) =>
        c.copy(gnuplot = c.gnuplot.copy(timeFormat = Some(timeFormat)))
      } text "Time format for gnuplot"

      opt[GnuplotFormat]("plot-output-format") action {(format,c) =>
        c.copy(gnuplot = c.gnuplot.copy(outputFormat = format))
      } text "Output format for gnuplot"

      opt[Unit]("strict") action {(_,c) =>
        c.copy(strict = true)
      } text "Fail on rubbish input instead the default of continuing to read"

      opt[Unit]("progress") action {(_,c) =>
        c.copy(progressMeter = new ConsoleProgressMeter())
      } text "Print progress to standard error. Only really sane if standard out is redirected."

      opt[Unit]("ui") action { (_, c) =>
        if (!Desktop.isDesktopSupported)
          throw new IllegalArgumentException("--ui only valid if a web browser can be opened")
        c.copy(liveUi = true)
      } text "Show output from an embedded web server"

      opt[Int]('n', "limit") action {(limit,c) =>
        c.copy(limit = Some(limit))
      } text "Only output n entries"

      opt[Unit]('F', "follow") action {(_,c) =>
        c.copy(follow = true, incremental = true)
      } text "Like tail -F. Read input as it's written to the file."

      opt[Unit]("merge") action {(_,c) =>
        c.copy(merge = true)
      } text "Remove duplicates records on input. Uses SHA-256 of record."

      opt[Unit]("timeline") action {(_,c) =>
        c.copy(format = JposTimeline())
      } text "Show jpos event timeline"

      opt[Unit]("debug") action {(_,c) =>
        Debug.enabled = true
        c
      } text "Show debug output"

      opt[Boolean]("highlight") action {(highlight,c) =>
        c.copy(format = if (highlight) HighlightedText else FullText)
      } text "Print with colours"
    }

    parser
    .parse(args, CmdLineOptions(LogTypes.auto(config, logTypes), format = null))
    .map { opts =>
      val format = opts.table match {
          // TODO: validate only 1 field when sqlIn
        case TableOptions(formatter, "*", contentType) =>
          val formatToUse = if (opts.incremental) formatter.liveVersion else formatter
          WildcardTable(formatToUse)
        case TableOptions(formatter, fields, contentType) if fields != "" =>
          val formatToUse = if (opts.incremental) formatter.liveVersion else formatter
          Tabular(parseFields(fields, contentType), formatToUse)
        case _ if opts.gnuplot.enabled =>
          Tabular(opts.gnuplot.fields)
        case _ => opts.format match {
          case null if opts.liveUi => FullText
          case null => if (canHandleAnsi) HighlightedText else FullText
          case f => f
        }
      }
      opts.copy(format = format)
    }
  }

  private def parseFields(fields: String, contentType: ContentType = PlainText): Seq[FieldExpr] = {
    fieldParser.copy(contentType = contentType).FieldExpr.expressionsFor(fields)
  }

  private def parseSortKey(key: String): Seq[SortKey] = {
    val ExprAndOrder = """([^ ]+)(?: (asc|desc))?""".r
    key.split(',').map {
      case ExprAndOrder(FieldExpr(expr), asc) => SortKey(expr, asc == "asc" || asc == null)
    }
  }

  private def defaultStrictFor(logType: LogType[LogEntry]): Boolean = logType match {
    case JposLog => false
    case _ => true
  }

  implicit def logFilterRead: Read[LogFilter] = new Read[LogFilter] {
    val arity = 2
    override def reads = {
      case LogFilter(f) => f
      case _ =>
        throw new IllegalArgumentException("Expected a key<op>value pair where <op> is one of =,<,>")
    }
  }

  implicit def nameTypeRead: Read[Option[NameType]] = new Read[Option[NameType]] {

    val arity = 1
    val reads = { (s: String) =>
      try {
        if (s == "") None else Some(NameType.withName(s))
      }
      catch {
        case e: NoSuchElementException => throw new IllegalArgumentException(s"Unknown name type '$s'. Known types are ${NameType.values.mkString(", ")}")
      }
    }
  }

  implicit def logTypeRead: Read[LogType[LogEntry]] = new Read[LogType[LogEntry]] {
    override def arity: Int = 1
    override def reads = { (s: String) =>
      logTypes.getOrElse(s, throw new IllegalArgumentException(s"Unknown input format '$s'. Known formats are ${logTypes.keys.mkString(", ")}"))
    }
  }

  implicit def gnuplotFormatRead: Read[GnuplotFormat] = new Read[GnuplotFormat] {
    override def arity: Int = 1
    override def reads = { (s: String) =>
      GnuplotFormat.from(s).getOrElse(throw new IllegalArgumentException(s"Unknown format '$s'. Known formats are ${GnuplotFormat.formats.mkString(", ")}"))
    }
  }

  implicit def tableFormatterRead: Read[TableFormatter] = new Read[TableFormatter] {
    override def arity: Int = 1
    override def reads = {
      case "tsv" | "t" => Tsv
      case "csv" | "c" => Csv
      case "ascii" => new AsciiTableFormat()
      case "html" => HtmlTableFormat
      case "jira" => JiraTableFormat
      case "sqlIn" => new SqlInClauseOutputFormat()
      case s =>
        throw new IllegalArgumentException(s"Unknown output format '$s'. Known formats are ${tableFormats.mkString(", ")}")
    }
  }

}
