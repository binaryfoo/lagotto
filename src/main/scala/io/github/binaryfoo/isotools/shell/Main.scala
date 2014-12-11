package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable
import io.github.binaryfoo.isotools.shell.FieldFilter.MatchOp
import io.github.binaryfoo.isotools.{ConvertibleToMap, LogReader}
import scopt.Read

object Main extends App {

  val parser = new scopt.OptionParser[Config]("plog") {
    head("plog", "1.0")

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
      c.copy(format = Tsv(fields.split(",")))
    } text "Output tab separated values"

    opt[String]('c', "csv") action { (fields, c) =>
      c.copy(format = Csv(fields.split(",")))
    } text "Output comma separated values"

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
  }

  parser.parse(args, Config()).map { config =>
    def logEntries = LogReader.readFilesOrStdIn(config.input)

    def pipeline: Stream[ConvertibleToMap] = {
      var entries: Stream[ConvertibleToMap] = if (config.pair)
        logEntries.pair()
      else
        logEntries

      if (config.header) {
        config.format.header().map(println(_))
      }

      if (config.filters.nonEmpty) {
        entries = entries.applyFilters(config.filters, config.beforeContext, config.afterContext)
      }

      // presumably both these options prevent incremental output
      if (config.sortBy != null) {
        entries = entries.sortBy(_(config.sortBy))
        if (config.sortDescending) {
          entries = entries.reverse
        }
      }
      entries
    }

      pipeline
      .map(e => config.format(e))
      .foreach(e => println(e))
  }

  implicit class RichLogEntries(val v: Stream[ConvertibleToMap]) extends AnyVal {

    def applyFilters(filters: Seq[LogFilter], beforeContext: Int, afterContext: Int): Stream[ConvertibleToMap] = {
      val shouldInclude = (m: ConvertibleToMap) => filters.forall(_(m))
      if (beforeContext == 0 && afterContext == 0) {
        // premature optimization for this case?
        v.filter(shouldInclude)
      } else {
        val preceding = new BoundedQueue[ConvertibleToMap](beforeContext)
        var aftersNeeded = 0
        v.flatMap { item =>
          if (shouldInclude(item)) {
            aftersNeeded = afterContext
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
  }

  implicit def logFilterRead: Read[LogFilter] = new Read[LogFilter] {
    def deNull(s: String): String = if (s == null) "" else s

    val LogFilterPattern = "([^=><~!]+)(!?)([=><~])(.*)".r
    val arity = 2
    val reads = { (s: String) =>
      s match {
        case LogFilterPattern(key, negation, operator, value) =>
          val op: MatchOp = operator match {
            case "=" => deNull(_) == _
            case ">" => _.toInt >= _.toInt
            case "<" => _.toInt <= _.toInt
            case "~" => deNull(_).toLowerCase contains _.toLowerCase
            case _ => throw new IllegalArgumentException("Expected a key<op>value pair where <op> is one of =,<,>")
          }
          if (negation == "!") {
            FieldFilter(key, value, (a, b) => !op(a, b))
          } else {
            FieldFilter(key, value, op)
          }
        case _ =>
          throw new IllegalArgumentException("Expected a key<op>value pair where <op> is one of =,<,>")
      }
    }
  }

}
