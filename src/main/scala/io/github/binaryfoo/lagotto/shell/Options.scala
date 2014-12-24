package io.github.binaryfoo.lagotto.shell

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

      opt[LogFilter]('f', "field") unbounded() action { case (filter, c) =>
        c.copy(filters = c.filters :+ filter)
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
        c.copy(sortBy = field)
      } text "Sort output by field. Prevents incremental output"

      opt[String]("sort-desc") action {(field,c) =>
        c.copy(sortBy = field, sortDescending = true)
      } text "Sort output descending by field. Prevents incremental output"

      opt[Unit]("strict") action {(_,c) =>
        c.copy(strict = true)
      } text "Fail on rubbish input instead the default of continuing to read"
    }

    parser.parse(args, Config())
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
}
