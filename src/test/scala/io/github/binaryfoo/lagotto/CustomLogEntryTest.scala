package io.github.binaryfoo.lagotto

import org.joda.time.DateTime

class CustomLogEntryTest extends LagoTest {

  val lineOne = """[16/Jan/2015 00:00:55 AEDT] 192.168.0.1 10.0.0.1:443 - - "GET /some/url HTTP/1.1" 200 + 262 1064959 "-" "UserAgent/1.0" TLSv1 RC4-SHA "-" "-" "-" "-" "-""""

  val pattern = """\[(?<timestamp>\d{2}/\w{3}/\d{4} \d{2}:\d{2}:\d{2} \w{3,4})\].* "(?<url>[^"]+)" (?<responseCode>\d{3}) [+-] [-0-9]+ (?<responseTime>\d+).*"""
  val parser = new CustomLogEntryParser(pattern, "dd/MMM/yyyy HH:mm:ss 'AEDT'")

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
}
