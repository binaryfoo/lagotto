package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto._
import scopt.Read

object Options {

  def parse(args: Array[String]): Option[Config] = {

    val parser = new scopt.OptionParser[Config]("plog") {
      val dog = "\uD83D\uDC15"
      head(s"lagotto $dog ", "1.0")

      help("help") text "Show usage"

      arg[String]("<log-file>...") unbounded() optional() action { (f, c) =>
        c.copy(input = c.input :+ f)
      } text "Optional list of log files"

      opt[String]('g', "grep") unbounded() action { (expr, c) =>
        c.copy(filters = c.filters :+ GrepFilter(expr))
      } text "Filter by messages including text"

      opt[String]("grep!") unbounded() action { (expr, c) =>
        c.copy(filters = c.filters :+ NegativeGrepFilter(expr))
      } text "Exclude messages containing text"

      opt[FieldFilter]('f', "field") unbounded() action { case (filter, c) =>
        if (filter.field == "delay") {
          c.copy(secondStageFilters = c.secondStageFilters :+ filter)
        } else {
          c.copy(filters = c.filters :+ filter)
        }
      } keyValueName ("path", "value") text "Filter by field path. Eg 48.1.2=value"

      opt[String]('t', "tsv") action { (fields, c) =>
        c.copy(format = Tabular(fields.split(","), DelimitedTableFormat("\t")))
      } text "Output tab separated values"

      opt[String]('c', "csv") action { (fields, c) =>
        c.copy(format = Tabular(fields.split(","), DelimitedTableFormat(",")))
      } text "Output comma separated values"

      opt[String]('j', "jira-table") action { (fields, c) =>
        c.copy(format = Tabular(fields.split(","), JiraTableFormat))
      } text "Output a table that can be pasted into Jira"

      opt[String]("html") action { (fields, c) =>
        c.copy(format = Tabular(fields.split(","), HtmlTableFormat))
      } text "Output an HTML table"

      opt[String]("ascii") action { (fields, c) =>
        c.copy(format = Tabular(fields.split(","), new AsciiTableFormat()))
      } text "Output an ASCII table"

      opt[String]("histogram") action { (fields, c) =>
        c.copy(histogramFields = fields.split(","))
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
        c.copy(sortBy = Some(field))
      } text "Sort output by field. Prevents incremental output"

      opt[String]("sort-desc") action {(field,c) =>
        c.copy(sortBy = Some(field), sortDescending = true)
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
    }

    parser.parse(args, Config())
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
}
