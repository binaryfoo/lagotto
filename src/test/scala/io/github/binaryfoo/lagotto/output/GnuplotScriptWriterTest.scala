package io.github.binaryfoo.lagotto.output

import io.github.binaryfoo.lagotto.shell.PlotStatementParser
import io.github.binaryfoo.lagotto.{SimpleLogEntry, FieldExprParser, LagoTest}

class GnuplotScriptWriterTest extends LagoTest {

  private val fieldExprParser = new FieldExprParser()
  private val parser = new PlotStatementParser(fieldExprParser)

  "One series with lines" should "be plotted" in {
    val input = "datetime vs (pause)"
    val (timeExpr, charts) = parser.parse(input)

    val script = GnuplotScriptWriter.write("foo.csv", ("00:00", "23:59"), GnuplotOptions(true, "foo", timeExpr, charts))
    script should include("plot csv using 1:2 w lines lt 2 t column(2)")
  }

  "One series with points" should "be plotted" in {
    val input = "datetime vs (pause) as points"
    val (timeExpr, charts) = parser.parse(input)

    val script = GnuplotScriptWriter.write("foo.csv", ("00:00", "23:59"), GnuplotOptions(true, "foo", timeExpr, charts))
    script should include("""plot csv using 1:2 w points lc 2 pt 2 ps 0.2 notitle,\
                            |     ''  using 1:(1/0) w points lc 2 pt 2 ps 1 t column(2)""".stripMargin)
  }

  "One series with labelled points" should "be plotted" in {
    val input = "datetime vs (pause) as points labelled by (type)"
    val (timeExpr, charts) = parser.parse(input)

    val discriminator = charts.head.series.head.chartType.asInstanceOf[DiscriminatedPoints]
    discriminator.field(SimpleLogEntry("type" -> "t1"))
    discriminator.field(SimpleLogEntry("type" -> "t 2"))

    val script = GnuplotScriptWriter.write("foo.csv", ("00:00", "23:59"), GnuplotOptions(true, "foo", timeExpr, charts))
    script should include("""plot for [t=1:2] csv using 1:($2 == t ? $3 : 1/0) w points lc t pt t ps 0.5 notitle,\
                            |     for [t=1:2] 1/0 w points lc t pt t ps 1 t word("'t1' 't 2'",t)""".stripMargin)
  }

  "Two series with same labelling condition on points" should "be plotted" in {
    val input = "datetime vs (A) as points labelled by (type), (B) as points labelled by (type)"
    val (timeExpr, charts) = parser.parse(input)

    val discriminator1 = charts.head.series.head.chartType.asInstanceOf[DiscriminatedPoints]
    discriminator1.field(SimpleLogEntry("type" -> "t1"))
    val discriminator2 = charts(1).series.head.chartType.asInstanceOf[DiscriminatedPoints]
    discriminator2.field(SimpleLogEntry("type" -> "t 2"))

    val script = GnuplotScriptWriter.write("foo.csv", ("00:00", "23:59"), GnuplotOptions(true, "foo", timeExpr, charts))
    script should include("""plot for [t=1:2] csv using 1:($2 == t ? $3 : 1/0) w points lc t pt t ps 0.5 notitle,\
                            |     for [t=1:2] 1/0 w points lc t pt t ps 1 t word("'t1' 't 2'",t)""".stripMargin)
    script should include("""plot for [t=1:2] csv using 1:($2 == t ? $4 : 1/0) w points lc t pt t ps 0.5 notitle,\
                            |     for [t=1:2] 1/0 w points lc t pt t ps 1 t word("'t1' 't 2'",t)""".stripMargin)
  }

  "4 series" should "be plotted" in {
    val input = "datetime vs (pause) as points labelled by (type), (heapBefore,heapAfter,heapMax) as points, (PSYoungGenBefore,PSYoungGenAfter,PSYoungGenMax) as points, (ParOldGenBefore,ParOldGenAfter,ParOldGenCapacityBefore)"
    val (timeExpr, charts) = parser.parse(input)

    val script = GnuplotScriptWriter.write("foo.csv", ("00:00", "23:59"), GnuplotOptions(true, "foo", timeExpr, charts))
    "(?m)^plot ".r.findAllIn(script).size shouldBe 4
  }
}
