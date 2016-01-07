package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.output._
import io.github.binaryfoo.lagotto._

import fastparse.all._

class PlotStatementParserTest extends LagoTest {

  private val fieldExprParser = new FieldExprParser()
  private val parser = new PlotStatementParser(fieldExprParser)

  private val A = PrimitiveExpr("A")
  private val B = PrimitiveExpr("B")
  private val C = PrimitiveExpr("C")

  "Time expression" should "be parsed" in {
    val (time, _) = parser.parse("time vs (A)")
    time shouldBe TimeExpr.unapply("time").get
  }

  "Single chart" should "be parsed" in {
    val (_, charts) = parser.parse("time vs (A,B,C)")
    charts shouldBe Seq(chart(Series(A, 2), Series(B, 3), Series(C, 4)))
  }

  "3 charts" should "be parsed" in {
    val (_, charts) = parser.parse("time vs (A),(B),(C)")
    charts shouldBe Seq(chart(Series(A, 2)), chart(Series(B, 3)), chart(Series(C, 4)))
  }

  "Space between charts" should "ok" in {
    val (_, charts) = parser.parse("time vs (B), (A),   (C)")
    charts shouldBe Seq(chart(Series(B, 2)), chart(Series(A, 3)), chart(Series(C, 4)))
  }

  "Aggregate dependent variable" should "be parsed" in {
    val countDistinct = fieldExprParser.stringAsFieldExpr("count(distinct)")
    val (_, charts) = parser.parse("time vs (count(distinct))")
    charts shouldBe Seq(chart(Series(countDistinct, 2)))
  }

  "Single chart as points" should "be parsed" in {
    val (_, charts) = parser.parse("time vs (A) as points")
    charts shouldBe Seq(chart(Series(A, 2, PointsPlot)))
  }

  "Single chart as lines" should "be parsed" in {
    val (_, charts) = parser.parse("time vs (A) as lines")
    charts shouldBe Seq(chart(Series(A, 2, LinesPlot)))
  }

  "Plot of each type" should "be parsed" in {
    val (_, charts) = parser.parse("time vs (C) as lines,(B,A) as points")
    charts shouldBe Seq(chart(Series(C, 2, LinesPlot)), chart(Series(B, 3, PointsPlot), Series(A, 4, PointsPlot)))
  }

  "Labelled points" should "be parsed" in {
    val (_, charts) = parser.parse("time vs (A) as points labelled by (B)")
    charts shouldBe Seq(chart(Series(A, 3, DiscriminatedPoints(OrdinalExpr("ordinal(B)", B), 2))))
  }

  "Multiple series in one chart with one labelling field" should "be parsed" in {
    val (time, charts) = parser.parse("time vs (A,B) as points labelled by (C)")
    val discriminator = DiscriminatedPoints(OrdinalExpr("ordinal(C)", C), 2)
    charts shouldBe Seq(chart(Series(A, 3, discriminator), Series(B, 4, discriminator)))

    GnuplotOptions(timeField = time, charts = charts).fields shouldBe Seq(time, OrdinalExpr("ordinal(C)", C), A, B)
  }

  "Multiple series in multiple charts with one labelling field" should "be parsed" in {
    val (time, charts) = parser.parse("time vs (A) as points labelled by (C),(B) as points labelled by (C)")
    val discriminator = DiscriminatedPoints(OrdinalExpr("ordinal(C)", C), 2)
    charts shouldBe Seq(chart(Series(A, 3, discriminator)), chart(Series(B, 4, discriminator)))

    GnuplotOptions(timeField = time, charts = charts).fields shouldBe Seq(time, OrdinalExpr("ordinal(C)", C), A, B)
  }

  "Plot field" should "parse until end bracket" in {
    val Parsed.Success(value, index) = PlotField.parse("A)")
    value shouldBe "A"
    index shouldBe 1
  }

  "Plot field" should "parse until end bracket from non-zero start" in {
    val Parsed.Success(value, index) = PlotField.parse("time vs (A)", 9)
    value shouldBe "A"
    index shouldBe 10
  }

  "Plot field" should "fail on premature end from non-zero start" in {
    val Parsed.Failure(_, 9, _) = PlotField.parse("time vs (", 9)
    val Parsed.Failure(_, 10, _) = PlotField.parse("time vs (A", 9)
  }

  "Plot field" should "fail on premature end" in {
    val Parsed.Failure(_, 0, _) = PlotField.parse("")
    val Parsed.Failure(_, 1, _) = PlotField.parse("A")
    val Parsed.Failure(_, 2, _) = PlotField.parse("A(")
  }

  def chart(series: Series*): Chart = Chart(Seq(series :_*))
}
