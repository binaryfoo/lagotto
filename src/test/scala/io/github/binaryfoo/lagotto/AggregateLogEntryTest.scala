package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.JposTimestamp.DateTimeExtension
import io.github.binaryfoo.lagotto.output.Xsv.SeqToXsv
import io.github.binaryfoo.lagotto.reader.FileIO
import org.joda.time.DateTime

class AggregateLogEntryTest extends LagoTest {

  private def threeStans = iteratorOver(
    JposEntry("0" -> "0200", "11" -> "1"),
    JposEntry("0" -> "0200", "11" -> "2"),
    JposEntry("0" -> "0210", "11" -> "3"))

  "Aggregation" should "support count" in {
    aggregateToCsv(threeStans, "mti", "count") shouldBe List("0200,2", "0210,1")
  }

  it should "support count(distinct(field))" in {
    aggregateToCsv(threeStans, "count(distinct(mti))") shouldBe List("2")
  }

  it should "support group_concat(distinct(field))" in {
    aggregateToCsv(threeStans, "group_concat(distinct(mti))") shouldBe List("0200,0210")
  }

  private def twoLifespans = iteratorOver(JposEntry(lines = "<log>1</log>", "lifespan" -> "100"), JposEntry(lines = "<log>2</log>", "lifespan" -> "200"))

  it should "support avg(field)" in {
    aggregate("avg(lifespan)") shouldBe List("150")
    aggregateToCsv(threeStans, "mti", "avg(11)") shouldBe List("0200,1", "0210,3")
  }

  it should "support min(field)" in {
    aggregate("min(lifespan)") shouldBe List("100")
    aggregateToCsv(threeStans, "mti", "min(11)") shouldBe List("0200,1", "0210,3")
  }

  it should "support max(field)" in {
    aggregate("max(lifespan)") shouldBe List("200")
    aggregateToCsv(threeStans, "mti", "max(11)") shouldBe List("0200,2", "0210,3")
  }

  it should "support sum(field)" in {
    aggregate("sum(lifespan)") shouldBe List("300")
    aggregateToCsv(threeStans, "mti", "sum(11)") shouldBe List("0200,3", "0210,3")
  }

  it should "support group_concat(field)" in {
    aggregate("group_concat(lifespan)") shouldBe List("100,200")
    aggregateToCsv(threeStans, "mti", "group_concat(11)") shouldBe List("0200,1,2", "0210,3")
  }

  it should "support group_sample(field N)" in {
    aggregate("group_sample(lifespan 1)") should (contain("100") or contain("200"))
    aggregateToCsv(threeStans, "mti", "group_sample(11 1)") should (contain("0210,3") and (contain("0200,1") or contain("0200,2")))
  }

  "group_trace" should "write a file per group" in {
    aggregateToCsv(twoLifespans, "lifespan", "group_trace(agg-test)") should (contain("100,agg-test.1.log") and contain("200,agg-test.2.log"))
    FileIO.readToString("agg-test.1.log") shouldBe "<log>1</log>\n"
    FileIO.readToString("agg-test.2.log") shouldBe "<log>2</log>\n"
    delete("agg-test.1.log")
    delete("agg-test.2.log")
  }

  private val now = new DateTime()
  private def threeLifespans = iteratorOver(
    JposEntry("at" -> now.asJposAt, "lifespan" -> "1000"),
    JposEntry("at" -> now.asJposAt, "lifespan" -> "2000"),
    JposEntry("at" -> now.asJposAt, "lifespan" -> "3000"))

  "Aggregation over a calculation" should "support max" in {
    aggregateToCsv(threeLifespans, "max(calc(timestamp-lifespan))") shouldBe Seq(DefaultDateTimeFormat.print(now.minusMillis(1000)))
  }

  it should "support min" in {
    aggregateToCsv(threeLifespans, "min(calc(timestamp-lifespan))") shouldBe Seq(DefaultDateTimeFormat.print(now.minusMillis(3000)))
  }

  it should "support avg" in {
    aggregateToCsv(threeLifespans, "avg(calc((time as millis)/(time as millis)))") shouldBe Seq("1")
  }

  it should "support sum" in {
    val expectedSum = threeLifespans.map(_.timestamp.getMillisOfDay).sum - 6000
    aggregateToCsv(threeLifespans, "sum((calc(timestamp-lifespan) time as millis))") shouldBe Seq(expectedSum.toString)
  }

  it should "support group_concat" in {
    val expectedGroup = (1 to 3).map(v => now.minusMillis(v * 1000).toString("HH:mm:ss")).mkString(",")
    aggregateToCsv(threeLifespans, "group_concat(calc(time(HH:mm:ss)-lifespan))") shouldBe Seq(expectedGroup)
  }

  it should "support count(distinct)" in {
    aggregateToCsv(threeLifespans, "count(distinct(calc(time(HH:mm:ss)-lifespan)))") shouldBe Seq("3")
  }

  it should "support group_concat(translate(70))" in {
    val twoMtis = iteratorOver(JposEntry("0" -> "0800"), JposEntry("0" -> "0810"))
    aggregateToCsv(parserWithRootDictionary, twoMtis, "group_concat(translate(0))") shouldBe Seq("Network Management Request,Network Management Response")
  }

  it should "support translate() as an aggregation key" in {
    val twoMtis = iteratorOver(JposEntry("0" -> "0800"), JposEntry("0" -> "0810"))
    aggregateToCsv(parserWithRootDictionary, twoMtis, "translate(0)", "count") shouldBe Seq("Network Management Request,1", "Network Management Response,1")
  }

  private def twoStrings = iteratorOver(
    JposEntry("48" -> "a"),
    JposEntry("48" -> "b"))

  "min(field)" should "support string comparison" in {
    aggregateToCsv(twoStrings, "min(48)") shouldBe List("a")
  }

  "max(field)" should "support string comparison" in {
    aggregateToCsv(twoStrings, "max(48)") shouldBe List("b")
  }

  "group_sample" should "parse" in {
    val expr = new FieldExprParser().FieldExpr.expressionFor("group_sample(line 3)")
    expr.toString() shouldBe "group_sample(line 3)"
    val op = expr.asInstanceOf[AggregateExpr].op.asInstanceOf[GroupSampleBuilder]
    op.size shouldEqual 3
    op.field shouldEqual "line"
  }

  it should "handle having fewer values than the sample size" in {
    val b = GroupSampleBuilder(PrimitiveExpr("mti"), 2)
    b.+=(JposEntry("0" -> "0200"))
    b.result() shouldBe "0200"
  }

  private def aggregate(field: String): List[String] = {
    aggregateToCsv(twoLifespans, field)
  }

  private def aggregateToCsv(raw: Iterator[LogEntry], fields: String*): List[String] = {
    aggregateToCsv(new FieldExprParser(), raw, fields :_*)
  }

  private def aggregateToCsv(parser: FieldExprParser, raw: Iterator[LogEntry], fields: String*): List[String] = {
    val fieldExprs = parser.FieldExpr.expressionsFor(fields)
    val aggregationConfig = AggregationSpec.fromExpressions(fieldExprs)
    val aggregated = AggregateExpr.aggregate(raw.toIterator, aggregationConfig.keys, aggregationConfig.aggregates.toSeq)
    aggregated.map(_.exprToSeq(fieldExprs).toCsv).toList
  }
}
