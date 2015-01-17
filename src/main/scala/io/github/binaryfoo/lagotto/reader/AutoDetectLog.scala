package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.{CustomLogEntryParser, LogEntry}

object AutoDetectLog extends LogType[LogEntry] {

  // TODO: not so custom if it's hard coded
  val apachePattern = """\[(?<timestamp>\d{2}/\w{3}/\d{4} \d{2}:\d{2}:\d{2} \w{3,4})\].* "(?<url>[^"]+)" (?<responseCode>\d{3}) [+-] [-0-9]+ (?<responseTime>\d+).*"""
  val apacheLog = new CustomLog(new CustomLogEntryParser(apachePattern, "dd/MMM/yyyy HH:mm:ss 'AEDT'"))

  override def apply(lines: SourceLineIterator): LogEntry = {
    if (lines.hasNext) {
      val first = lines.peek()
      if (first.charAt(0) == '[')
        if (first.charAt(3) == '/') apacheLog(lines)
        else Log4jLog(lines)
      else
        JposLog(lines)
    } else {
      null
    }
  }
}
