package io.github.binaryfoo.isotools.shell

import io.github.binaryfoo.isotools.MsgPair.RichEntryIterable
import io.github.binaryfoo.isotools.{ConvertibleToMap, LogReader}

object Main extends App {

  val parser = new scopt.OptionParser[Config]("plog") {
    head("plog", "1.0")

    help("help") text "Show usage"

    arg[String]("<log-file>...") unbounded() optional() action { (f, c) =>
      c.copy(input = c.input :+ f)
    } text "Optional list of log files"

    opt[String]('g', "grep") unbounded() action { (expr, c) =>
      c.copy(filters = c.filters :+ GrepFilter(expr))
    }

    opt[(String, String)]('f', "field") unbounded() action { case ((path, value), c) =>
      c.copy(filters = c.filters :+ FieldFilter(path, value))
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
  }

  parser.parse(args, Config()).map { config =>
    val logEntries = LogReader.readFilesOrStdIn(config.input)

    val entries: Stream[ConvertibleToMap] = if (config.pair)
      logEntries.pair()
    else
      logEntries

    entries
      .applyFilters(config.filters)
      .map(e => config.format(e))
      .foreach(e => println(e))
  }

  implicit class RichLogEntries(val v: Stream[ConvertibleToMap]) extends AnyVal {
    def applyFilters(filters: Seq[LogFilter]): Stream[ConvertibleToMap] = {
      filters.foldLeft(v) { (s, filter) =>
        s.filter(filter(_))
      }
    }
  }
}
