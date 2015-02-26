package io.github.binaryfoo.lagotto

import java.io.File

import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import org.joda.time.{DateTime, LocalTime}

import scala.collection.mutable

class FieldExprTest extends LagoTest {

  val parser = new FieldExprParser()
  import parser.FieldExpr
  import parser.FieldExpr._

  "calc(max(a)-min(a))" should "diff two times" in {
    val expr = expressionFor("calc(max(time)-min(time))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(time)" -> "03:03:03.333", "min(time)" -> "02:02:02.222")))
    diff shouldBe "01:01:01.111"
    expr.toString() shouldBe "calc(max(time)-min(time))"
  }

  it should "always show zero for times" in {
    val expr = expressionFor("calc(max(time)-min(time))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(time)" -> "02:02:02.222", "min(time)" -> "02:02:02.222")))
    diff shouldBe "00:00:00.000"
  }

  it should "handle custom time format HH:m0" in {
    val expr = expressionFor("calc(max(time(HH:m0))-min(time(HH:m0)))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(time(HH:m0))" -> "03:30", "min(time(HH:m0))" -> "02:20")))
    diff shouldBe "01:10"
  }

  it should "handle full date with difference of days" in {
    val expr = expressionFor("calc(max(date)-min(date))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(date)" -> "2015-01-01", "min(date)" -> "2014-12-31")))
    diff shouldBe "1 day"
  }

  it should "handle full date with difference of months" in {
    val expr = expressionFor("calc(max(date)-min(date))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(date)" -> "2015-01-01", "min(date)" -> "2014-11-30")))
    diff shouldBe "1 month 2 days"
  }

  it should "handle full date with difference of years" in {
    val expr = expressionFor("calc(max(date)-min(date))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(date)" -> "2017-01-01", "min(date)" -> "2014-10-30")))
    diff shouldBe "2 years 2 months 2 days"
  }

  it should "handle full timestamp with difference of days" in {
    val expr = expressionFor("calc(max(timestamp)-min(timestamp))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(timestamp)" -> "2015-01-01 17:03:33.333", "min(timestamp)" -> "2014-12-31 16:02:22.222")))
    diff shouldBe "1 day 01:01:11.111"
  }

  it should "handle full timestamp with difference of months" in {
    val expr = expressionFor("calc(max(timestamp)-min(timestamp))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(timestamp)" -> "2015-01-01 17:03:33.333", "min(timestamp)" -> "2014-11-30 16:02:22.222")))
    diff shouldBe "1 month 2 days 01:01:11.111"
  }

  it should "handle full timestamp with difference of years" in {
    val expr = expressionFor("calc(max(timestamp)-min(timestamp))")
    val diff = expr(AggregateLogEntry(Map(), Seq("max(timestamp)" -> "2015-01-01 17:03:33.333", "min(timestamp)" -> "2013-10-30 16:02:22.222")))
    diff shouldBe "1 year 2 months 2 days 01:01:11.111"
  }

  "calc(timestamp-lifespan)" should "show when log event was created" in {
    val loggedAt = new DateTime()
    val expr = expressionFor("calc(timestamp-lifespan)")
    expr(JposEntry("at" -> loggedAt.asJposAt, "lifespan" -> "1000")) shouldBe DefaultDateTimeFormat.print(loggedAt.minusMillis(1000))
  }

  it should "work for aggregation output (different because the individual fields aren't available)" in {
    val expr = expressionFor("calc(timestamp-lifespan)")
    expr(AggregateLogEntry(Map("calc(timestamp-lifespan)" -> "already computed"), Seq())) shouldBe "already computed"
  }

  "calc(lifespan-rtt)" should "show integer difference" in {
    val expr = expressionFor("calc(lifespan-rtt)")
    expr(JposEntry("lifespan" -> "1000", "rtt" -> "300")) shouldBe "700"
  }

  it should "return null if one side is missing" in {
    val expr = expressionFor("calc(lifespan-rtt)")
    expr(JposEntry("lifespan" -> "1000")) shouldBe null
    expr(JposEntry("rtt" -> "300")) shouldBe null
    expr.get(JposEntry("rtt" -> "300")) shouldBe None
  }

  "calc(max(lifespan)-max(rtt))" should "show integer difference" in {
    val expr = expressionFor("calc(max(lifespan)-max(rtt))")
    expr(AggregateLogEntry(Map.empty, Seq("max(lifespan)" -> "1000", "max(rtt)" -> "330"))) shouldBe "670"
    expr.toString() shouldBe "calc(max(lifespan)-max(rtt))"
  }

  "alias" should "wrap target expression and rename it" in {
    val expr = expressionFor("calc(max(lifespan)-max(rtt)) as \"delta\"")
    expr(AggregateLogEntry(Map.empty, Seq("max(lifespan)" -> "1000", "max(rtt)" -> "330"))) shouldBe "670"
    expr.toString() shouldBe "delta"
  }

  "Format expression (lifespan millis as period)" should "convert millis to a time period" in {
    val expr = expressionFor("(lifespan millis as period)")
    expr(JposEntry("lifespan" -> "3600000")) shouldBe "01:00:00.000"
  }

  it should "handle missing value" in {
    val expr = expressionFor("(lifespan millis as period)")
    expr(JposEntry()) shouldBe null
  }

  "(lifespan millis as time(HH:mm))" should "convert millis to a time period with only hour and minute fields" in {
    val expr = expressionFor("(lifespan millis as time(HH:mm))")
    expr(JposEntry("lifespan" -> "3600000")) shouldBe "01:00"

    val shortExpr = expressionFor("(lifespan ms as time(HH:mm))")
    shortExpr(JposEntry("lifespan" -> "3600000")) shouldBe "01:00"
  }

  "(max(lifespan) millis as time(HH:mm))" should "confess to containing an aggregate expression" in {
    val expr = expressionFor("(max(lifespan) millis as time(HH:mm))")
    val aggregates = expr match { case HasAggregateExpressions(children) => children }
    aggregates shouldBe Seq(expressionFor("max(lifespan)"))
  }

  "(time(HH:mm) as millis)" should "convert time to millis period" in {
    val expr = expressionFor("(time(HH:mm) as millis)")
    expr(JposEntry("at" -> new LocalTime(1, 0).toDateTimeToday.asJposAt)) shouldBe "3600000"

    val shortExpr = expressionFor("(time(HH:mm) as ms)")
    shortExpr(JposEntry("at" -> new LocalTime(1, 0).toDateTimeToday.asJposAt)) shouldBe "3600000"
  }

  "(max(responseTime) micro as seconds)" should "convert microseconds to seconds" in {
    val expr = expressionFor("(max(responseTime) micro as seconds)")
    expr(AggregateLogEntry(Map(), Seq("max(responseTime)" -> "20105046"))) shouldBe "20.1"

    val shortExpr = expressionFor("(max(responseTime) us as s)")
    shortExpr(AggregateLogEntry(Map(), Seq("max(responseTime)" -> "20105046"))) shouldBe "20.1"
  }

  "(max(responseTime) micro as millis)" should "convert microseconds to millis" in {
    val expr = expressionFor("(max(responseTime) micro as millis)")
    expr(AggregateLogEntry(Map(), Seq("max(responseTime)" -> "20005046"))) shouldBe "20005"

    val shortExpr = expressionFor("(max(responseTime) us as ms)")
    shortExpr(AggregateLogEntry(Map(), Seq("max(responseTime)" -> "20005046"))) shouldBe "20005"
  }

  "(max(responseTime) millis as seconds)" should "convert millis to seconds" in {
    val expr = expressionFor("(max(responseTime) millis as seconds)")
    expr(AggregateLogEntry(Map(), Seq("max(responseTime)" -> "21005"))) shouldBe "21"

    val shortExpr = expressionFor("(max(responseTime) ms as s)")
    shortExpr(AggregateLogEntry(Map(), Seq("max(responseTime)" -> "21005"))) shouldBe "21"
  }

  "(calc(timestamp-lifespan) as millis)" should "convert the calc output to millis period" in {
    val expr = expressionFor("(calc(timestamp-lifespan) time as millis)")
    expr(JposEntry("at" -> new LocalTime(1, 0).toDateTimeToday.asJposAt, "lifespan" -> "60000")) shouldBe "3540000"
  }

  "(lifespan millis as seconds)" should "include one dp of millis" in {
    val expr = expressionFor("(lifespan millis as seconds)")
    expr(JposEntry("lifespan" -> "12300")) shouldBe "12.3"
    expr(JposEntry("lifespan" -> "12390")) shouldBe "12.4"
    expr(JposEntry("lifespan" -> "1002310")) shouldBe "1002.3"
    expr(JposEntry("lifespan" -> "1002010")) shouldBe "1002"
  }

  "(time(HH:mm) as peanuts)" should "complain about unknown conversion" in {
    the [IAmSorryDave] thrownBy {
      FieldExpr.unapply("(time(HH:mm) as peanuts)")
    } should have message "Unknown conversion (time(HH:mm) as peanuts)"
  }

  "(11 as int)" should "drop leading zeroes" in {
    val expr = expressionFor("(11 as int)")
    expr(JposEntry("11" -> "000001")) shouldBe "1"
    expr(JposEntry("11" -> "100000")) shouldBe "100000"
  }

  "(thing as href)" should "create an HTML anchor tag" in {
    val expr = expressionFor("(thing as href)")
    expr(JposEntry("thing" -> "file.txt")) should fullyMatch regex """<a href="file.txt".*</a>"""
  }

  "xpath(expr)" should "evaluate the xpath expression over log text" in {
    val expr = expressionFor("xpath(//jobs[text()])")
    val entry = JposEntry.fromLines(linesFrom("pool-exhaustion.xml"))
    expr(entry) shouldEqual "12413"
  }

  "Format expression (calc(time-lifespan) time as time(HH:mm:ss))" should "drop millis from the modified timestamp" in {
    val loggedAt = new DateTime()
    val expr = expressionFor("(calc(time-lifespan) time as time(HH:mm:ss))")
    expr(JposEntry("at" -> loggedAt.asJposAt, "lifespan" -> "1000")) shouldBe loggedAt.minusMillis(1000).toString("HH:mm:ss")
  }

  it should "work for aggregation output (different because the individual fields aren't available)" in {
    val expr = expressionFor("(calc(time-lifespan) time as time(HH:mm:ss))")
    expr(AggregateLogEntry(Map("(calc(time-lifespan) time as time(HH:mm:ss))" -> "already computed"), Seq())) shouldBe "already computed"
  }

  "calc(count(mti=0200)/count)" should "calculate percentage of 0200's" in {
    val expr = expressionFor("calc(count(mti=0200)/count)")
    val percent = expr(AggregateLogEntry(Map(), Seq("count(mti=0200)" -> "50", "count" -> "100")))
    percent shouldBe "0.5000"
  }

  "calc(4/lifespan)" should "perform a rather useless division" in {
    val expr = expressionFor("calc(4/lifespan)")
    val percent = expr(JposEntry("4" -> "000000050", "lifespan" -> "200"))
    percent shouldBe "0.2500"
  }

  "calc(a-b)" should "whinge on an attempt use an aggregate and a direct field" in {
    the [IAmSorryDave] thrownBy {
      FieldExpr.unapply("calc(min(time)-lifespan)")
    } should have message "In calc(left-right) both sides must be aggregate or direct operations. Left: min(time) and Right: lifespan are not compatible."

    an [IAmSorryDave] should be thrownBy  {
      FieldExpr.unapply("calc(time-min(lifespan))")
    }
  }

  "calc(a/b)" should "whinge on an attempt to use an aggregate and a direct field" in {
    the [IAmSorryDave] thrownBy {
      FieldExpr.unapply("calc(avg(time)/lifespan)")
    } should have message "In calc(left/right) both sides must be aggregate or direct operations. Left: avg(time) and Right: lifespan are not compatible."

    an [IAmSorryDave] should be thrownBy  {
      FieldExpr.unapply("calc(time/sum(lifespan))")
    }
  }

  "calc(time()" should "whinge on an attempt to use an aggregate and a direct field" in {
    the [IAmSorryDave] thrownBy {
      FieldExpr.unapply("calc(avg(time)/lifespan)")
    } should have message "In calc(left/right) both sides must be aggregate or direct operations. Left: avg(time) and Right: lifespan are not compatible."

    an [IAmSorryDave] should be thrownBy  {
      FieldExpr.unapply("calc(time/sum(lifespan))")
    }
  }

  "Primitive field access" should "fall back to dictionary lookup" in {
    val expr = parserWithRootDictionary.FieldExpr.expressionFor("stan")
    expr(JposEntry("11" -> "123456")) shouldBe "123456"
  }

  "translate(70)" should "show translated value" in {
    val expr = parserWithRootDictionary.FieldExpr.expressionFor("translate(70)")
    expr(JposEntry("0" -> "0800", "70" -> "301")) shouldBe "Echo"
  }

  "translate(nmic)" should "show translated value" in {
    val expr = parserWithRootDictionary.FieldExpr.expressionFor("translate(nmic)")
    expr(JposEntry("0" -> "0800", "70" -> "301")) shouldBe "Echo"
  }

  "48.max" should "pick max immediate child of 48 in jpos entries" in {
    val expr = expressionFor("48.max")
    expr(JposEntry("48.1" -> "one", "48.2" -> "two")) shouldBe "two"
  }

  it should "handle plain old 48" in {
    val expr = expressionFor("48.max")
    expr(JposEntry("48" -> "no subfields")) shouldBe null
  }

  it should "do a simple lookup on other record types" in {
    val expr = expressionFor("48.max")
    expr(AggregateLogEntry(Map("48.max" -> "value"), Seq())) shouldBe "value"
  }

  it should "allow regex replacement" in {
    val expr = expressionFor("48.max(/ \\$.*//)")
    expr(JposEntry("48.2" -> "Purchase $135.98")) shouldBe "Purchase"
    expr(JposEntry("48.2" -> "No Purchase")) shouldBe "No Purchase"
  }

  "48.max.1" should "pick subfield 1 of max immediate child of 48" in {
    val expr = expressionFor("48.max.1")
    expr(JposEntry("48.1.1" -> "one", "48.3.1" -> "two")) shouldBe "two"
  }

  "48.min" should "pick min immediate child of 48" in {
    val expr = expressionFor("48.min")
    expr(JposEntry("48.1" -> "one", "48.2" -> "two")) shouldBe "one"
  }

  "48.min.1" should "pick subfield 1 of min immediate child of 48" in {
    val expr = expressionFor("48.min.1")
    expr(JposEntry("48.1.1" -> "one", "48.3.1" -> "two")) shouldBe "one"
  }

  "48.*" should "concatenate all children of 48" in {
    val expr = expressionFor("48.*")
    expr(JposEntry("48.1" -> "one", "48.1.1" -> "one.one", "48.2" -> "two")) shouldBe "one,one.one,two"
  }

  val dummy455 = SimpleLogEntry(new mutable.LinkedHashMap[String, String](), lines = "", source = FileRef(new File("/dummy/path.log"), 455))

  "file" should "show file name" in {
    val expr = expressionFor("file")
    expr(dummy455) shouldBe "path.log"
  }

  "line" should "show line number" in {
    val expr = expressionFor("line")
    expr(dummy455) shouldBe "455"
  }

  "src" should "show file name and line number" in {
    val expr = expressionFor("src")
    expr(dummy455) shouldBe "path.log:455"
  }

  "src with Html render hint" should "create html anchor for source file:// URL" in {
    val expr = parser.copy(renderHints = Set(RenderHint.Html)).FieldExpr.expressionFor("src")
    expr(dummy455) shouldBe """<a href="/dummy/path.log?from=454&to=455" title="path.log:455">&#9906;</a>"""
  }

  it should "not create a link for records from stdin" in {
    val expr = expressionFor("src")
    val entry = SimpleLogEntry(new mutable.LinkedHashMap[String, String](), lines = "", source = StdInRef(356))
    expr(entry) shouldBe "stdin:356"
  }

  "icon" should "use msgType for rich html" in {
    val htmlize = (x: String) => "&#" + x(0).toInt + ";"
    val expr = parser.copy(renderHints = Set(RenderHint.Html)).FieldExpr.expressionFor("icon")
    expr(JposEntry("msgType" -> "send")) shouldBe htmlize("→")
    expr(JposEntry("msgType" -> "receive")) shouldBe htmlize("←")
    expr(JposEntry("msgType" -> "session-start")) shouldBe htmlize("↦")
    expr(JposEntry("msgType" -> "session-end")) shouldBe htmlize("✗")
    expr(JposEntry("msgType" -> "session-error")) shouldBe htmlize("☠")
    expr(JposEntry("msgType" -> "peer-disconnect")) shouldBe htmlize("☄")
    expr(JposEntry("msgType" -> "io-timeout")) shouldBe htmlize("⏰")
  }

  "icon" should "use msgType for rich text" in {
    val expr = parser.copy(renderHints = Set(RenderHint.RichText)).FieldExpr.expressionFor("icon")
    expr(JposEntry("msgType" -> "send")) shouldBe "→"
    expr(JposEntry("msgType" -> "receive")) shouldBe "←"
    expr(JposEntry("msgType" -> "session-start")) shouldBe "↦"
    expr(JposEntry("msgType" -> "session-end")) shouldBe "✗"
    expr(JposEntry("msgType" -> "session-error")) shouldBe "☠"
    expr(JposEntry("msgType" -> "peer-disconnect")) shouldBe "☄"
    expr(JposEntry("msgType" -> "io-timeout")) shouldBe "⏰"
  }

  it should "use msgType for text" in {
    val expr = expressionFor("icon")
    expr(JposEntry("msgType" -> "send")) shouldBe "->"
    expr(JposEntry("msgType" -> "receive")) shouldBe "<-"
    expr(JposEntry("msgType" -> "session-start")) shouldBe "[~"
    expr(JposEntry("msgType" -> "session-end")) shouldBe "~]"
    expr(JposEntry("msgType" -> "session-error")) shouldBe "!!"
    expr(JposEntry("msgType" -> "peer-disconnect")) shouldBe "X"
    expr(JposEntry("msgType" -> "io-timeout")) shouldBe "T"
  }

  "pivot()" should "collect distinct values" in {
    val expr = expressionFor("pivot(mti)")
    expr(JposEntry("0" -> "0200")) shouldBe "0200"
    expr(JposEntry("0" -> "0200")) shouldBe "0200"
    expr(JposEntry("0" -> "0210")) shouldBe "0210"
    expr.asInstanceOf[PivotExpr].distinctValues() shouldBe Seq("0200", "0210")
  }

  "field(/regex/replacement/)" should "return regex replacement" in {
    val expr = expressionFor("11(/0+([1-9]+)/x $1 x/)")
    expr(JposEntry("11" -> "000003")) shouldBe "x 3 x"
  }

  it should "apply regex to result of another expression" in {
    val expr = expressionFor("src(/.log//)")
    expr(dummy455) shouldBe "path:455"
  }

  "time expressions" should "return timestamp" in {
    expressionFor("time(HH:mm)")(JposEntry("at" -> new LocalTime(10, 30).toDateTimeToday.asJposAt)) shouldBe "10:30"
    expressionFor("time(HH:m0)")(JposEntry("at" -> new LocalTime(10, 33).toDateTimeToday.asJposAt)) shouldBe "10:30"
  }

  "delay" should "return DelayExpr" in {
    expressionFor("delay") shouldEqual DelayExpr
  }
}
