package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.dictionary.NameType.NameType
import io.github.binaryfoo.lagotto.dictionary.{NameType, DataDictionary}
import io.github.binaryfoo.lagotto.shell.output.{AsciiTableFormat, DigestedFormat, IncrementalAsciiTableFormat, JSONOutput}
import scopt.Read

object Options {

  def parse(args: Array[String], dictionary: DataDictionary): Option[Config] = {

    val parser = new scopt.OptionParser[Config]("plog") {
      head(s"lagotto", "0.0.1")

      help("help") text "Show usage"

      arg[String]("<log-file>...") unbounded() optional() action { (f, c) =>
        c.copy(input = c.input :+ f)
      } text "Optional list of log files. Otherwise read stdin."

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

      opt[FieldFilter]('f', "field") unbounded() action { case (filter, c) =>
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
        c.copy(format = Tabular(parseFields(fields), HtmlTableFormat))
      } text "Output an HTML table"

      opt[String]("ascii") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), new AsciiTableFormat()))
      } text "Output an ASCII table"

      opt[String]("live-ascii") action { (fields, c) =>
        c.copy(format = Tabular(parseFields(fields), new IncrementalAsciiTableFormat()))
      } text "Output an ASCII table incrementally. Can be messy."

      opt[Unit]("json") action { (_, c) =>
        c.copy(format = JSONOutput(dictionary))
      } text "Output a line of JSON per log entry."

      opt[Unit]("digest") action { (_, c) =>
        c.copy(format = DigestedFormat(dictionary, NameType.English))
      } text "Output full message in a compact format."

      opt[NameType]("digest-as") action { (nameType, c) =>
        c.copy(format = DigestedFormat(dictionary, nameType))
      } text "Output full message in a compact format with name type: English, Short or Export."

      opt[String]("histogram") action { (fields, c) =>
        c.copy(histogramFields = FieldExpr.expressionsFor(fields))
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
        c.copy(sortBy = FieldExpr.unapply(field))
      } text "Sort output by field. Prevents incremental output"

      opt[String]("sort-desc") action {(field,c) =>
        c.copy(sortBy = FieldExpr.unapply(field), sortDescending = true)
      } text "Sort output descending by field. Prevents incremental output"

      opt[String]("gnuplot") action {(fileName,c) =>
        c.copy(gnuplotFileName = Some(fileName))
      } text "Write a gnuplot script <name>.gp. Write the output to <name>.csv. Only makes sense with --tsv."

      opt[Unit]("strict") action {(_,c) =>
        c.copy(strict = true)
      } text "Fail on rubbish input instead the default of continuing to read"

      opt[Unit]("progress") action {(_,c) =>
        c.copy(progressMeter = new ConsoleProgressMeter())
      } text "Print progress to standard error. Only really sane if standard out is redirected."

      opt[Int]('n', "limit") action {(limit,c) =>
        c.copy(limit = Some(limit))
      } text "Only output n entries"
    }

    parser.parse(args, Config())
  }

  def parseFields(fields: String): Seq[FieldExpr] = {
    fields.split(",").map { case FieldExpr(e) => e }
  }

  implicit def logFilterRead: Read[FieldFilter] = new Read[FieldFilter] {
    val arity = 2
    val reads = { (s: String) =>
      s match {
        case LogFilter(f) => f
        case _ =>
          throw new IllegalArgumentException("Expected a key<op>value pair where <op> is one of =,<,>")
      }
    }
  }

  implicit def nameTypeRead: Read[NameType] = new Read[NameType] {

    val arity = 1
    val reads = { (s: String) =>
      try {
        NameType.withName(s)
      }
      catch {
        case e: NoSuchElementException => throw new IllegalArgumentException(s"Unknown name type '$s'. Known types are ${NameType.values.mkString(", ")}")
      }
    }
  }
}
