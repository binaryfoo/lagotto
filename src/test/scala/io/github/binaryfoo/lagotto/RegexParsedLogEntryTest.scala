package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.reader.{SourceLineIterator, GcLogLineRecogniser, RegexParsedLog, AutoDetectLog}
import org.joda.time.DateTime

class RegexParsedLogEntryTest extends LagoTest {

  val lineOne = """[16/Jan/2015 00:00:55 AEDT] 192.168.0.1 10.0.0.1:443 - - "GET /some/url HTTP/1.1" 200 + 262 1064959 "-" "UserAgent/1.0" TLSv1 RC4-SHA "-" "-" "-" "-" "-""""

  val pattern = """\[(?<timestamp>\d{2}/\w{3}/\d{4} \d{2}:\d{2}:\d{2} \w{3,4})\].* "(?<url>[^"]+)" (?<responseCode>\d{3}) [+-X] [-0-9]+ (?<responseTime>\d+).*"""
  val parser = new RegexParsedLog(pattern, "dd/MMM/yyyy HH:mm:ss 'AEDT'")

  "Apache log format" should "be parseable" in {
    val entry = parser.fromString(lineOne)
    entry("timestamp") shouldBe "16/Jan/2015 00:00:55 AEDT"
    entry("url") shouldBe "GET /some/url HTTP/1.1"
    entry("responseCode") shouldBe "200"
    entry("responseTime") shouldBe "1064959"
  }

  it should "parse timestamp" in {
    val entry = parser.fromString(lineOne)
    entry.timestamp shouldBe new DateTime(2015, 1, 16, 0, 0, 55, 0)
  }

  it should "parse X" in {
    val line = """[16/Jan/2015 01:07:46 AEDT] 192.168.0.1 10.0.0.1:443 - - "GET /some/url HTTP/1.1" 404 X 1507 20005046 "-" "UserAgent/0.9" TLSv1 RC4-SHA "-" "-" "-" "-" "-""""
    val entry = parser.fromString(line)
    entry("timestamp") shouldBe "16/Jan/2015 01:07:46 AEDT"
  }

  "GcLogLineRecogniser" should "ignore boring lines" in {
    val log = new RegexParsedLog(".*", "yyyy-MM-dd", GcLogLineRecogniser)
    val iterator = new SourceLineIterator(Seq("one", "2015 real=", "three").iterator, "", false, false)
    val lineSet = log.readLinesForNextRecord(iterator)
    lineSet.fullText shouldBe "2015 real="

    log.readLinesForNextRecord(iterator) shouldBe null
  }
}
