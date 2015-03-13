package io.github.binaryfoo.lagotto.shell

import java.awt.Desktop

import com.typesafe.config.Config
import io.github.binaryfoo.lagotto.RenderHint.RenderHint
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType
import io.github.binaryfoo.lagotto.dictionary.{NameType, RootDataDictionary}
import io.github.binaryfoo.lagotto.reader.{JposLog, LogType, LogTypes}
import io.github.binaryfoo.lagotto.shell.output._
import scopt.Read

class OptionsParser(val config: Config) {

  val dictionary = RootDataDictionary(config)
  val logTypes = LogTypes.load(config)
  val fieldParser = new FieldExprParser(Some(dictionary))
  val filterParser = new LogFilterParser(fieldParser)
  import fieldParser.FieldExpr
  import filterParser.LogFilter

  def parse(args: Array[String]): Option[CmdLineOptions] = {

    val canHandleAnsi = IsATty()

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

      opt[String]('t', "tsv") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), DelimitedTableFormat("\t")))
      } text "Output tab separated values"

      opt[String]('c', "csv") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), DelimitedTableFormat(",")))
      } text "Output comma separated values"

      opt[String]('j', "jira-table") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), JiraTableFormat))
      } text "Output a table that can be pasted into Jira"

      opt[String]("html") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields, Set(RenderHint.Html)), HtmlTableFormat))
      } text "Output an HTML table"

      opt[String]("ascii") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), new AsciiTableFormat()))
      } text "Output an ASCII table"

      opt[String]("utf") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields, Set(RenderHint.RichText)), new AsciiTableFormat()))
      } text "Same as --ascii but uses symbol characters that might not render"

      opt[String]("live-ascii") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), new IncrementalAsciiTableFormat()))
      } text "Output an ASCII table incrementally. Can be messy."

      opt[String]("live-utf") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields, Set(RenderHint.RichText)), new IncrementalAsciiTableFormat()))
      } text "Same as --live-ascii but uses symbol characters that might not render"

      opt[String]("sqlIn") maxOccurs 1 action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), new SqlInClauseOutputFormat()))
      } text "Output a SQL IN statement for the results"

      opt[Unit]("json") action { (_, c) =>
        c.copy(format = JSONOutput(dictionary))
      } text "Output a line of JSON per log entry."

      opt[Unit]("digest") action { (_, c) =>
        c.copy(format = DigestedFormat(dictionary, Some(NameType.English)))
      } text "Output full message in a compact format."

      opt[Unit]("names") action { (_, c) =>
        c.copy(format = NamedAttributesFormat(dictionary))
      } text "XML with extra name=\"\" attribute for each <field/>"

      opt[Option[NameType]]("digest-as") action { (nameType, c) =>
        c.copy(format = DigestedFormat(dictionary, nameType))
      } text "Output full message in a compact format with name type: English, Short or Export."

      opt[String]("histogram") action { (fields, c) =>
        c.copy(histogramFields = parseFields(fields))
      } text "Output a histogram"

      opt[Unit]("pair") action {(_, c) =>
        c.copy(pair = true)
      } text "Match requests with responses"

      opt[Unit]("no-header") action {(_, c) =>
        c.copy(header = false)
      } text "Don't print the tsv/csv header row"

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
        c.copy(gnuplotFileName = Some(fileName))
      } text "Write a gnuplot script <name>.gp. Write the output to <name>.csv. Only makes sense with --tsv."

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
        c.copy(follow = true)
      } text "Like tail -F. Read input as it's written to the file."

      opt[Unit]("merge") action {(_,c) =>
        c.copy(merge = true)
      } text "Remove duplicates records on input. Uses SHA-256 of record."

      opt[Unit]("debug") action {(_,c) =>
        Debug.enabled = true
        c
      } text "Show debug output"

      opt[Boolean]("highlight") action {(highlight,c) =>
        c.copy(format = if (highlight) HighlightedText else FullText)
      } text "Print with colours"
    }

    val autoDetectLog = LogTypes.auto(config, logTypes)
    val defaultOutput = if (canHandleAnsi) HighlightedText else FullText
    parser.parse(args, CmdLineOptions(autoDetectLog, format = defaultOutput))
  }

  private def parseFields(fields: String, renderHints: Set[RenderHint] = Set.empty): Seq[FieldExpr] = {
    fieldParser.copy(renderHints = renderHints).FieldExpr.expressionsFor(fields)
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
    val reads = { (s: String) =>
      s match {
        case LogFilter(f) => f
        case _ =>
          throw new IllegalArgumentException("Expected a key<op>value pair where <op> is one of =,<,>")
      }
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
}
