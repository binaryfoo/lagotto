package io.github.binaryfoo.lagotto.output

import io.github.binaryfoo.lagotto.{OrdinalExpr, FieldExpr, MsgPairFieldAccess}

import scala.annotation.tailrec

sealed trait ColumnGrouping
object ChartPerColumn extends ColumnGrouping
object SingleChart  extends ColumnGrouping
case class ChartPerCluster(clusters: Seq[Seq[Int]]) extends ColumnGrouping

sealed trait PlotType
object LinesPlot extends PlotType {
  override def toString: String = "LinesPlot"
}
object PointsPlot extends PlotType {
  override def toString: String = "PointsPlot"
}
case class DiscriminatedPoints(field: OrdinalExpr, columnNumber: Int) extends PlotType with NumberedField

case class Series(field: FieldExpr, columnNumber: Int, chartType: PlotType = LinesPlot) extends NumberedField

trait NumberedField {
  def field: FieldExpr
  def columnNumber: Int
}

case class Chart(series: Seq[Series])

case class GnuplotOptions(enabled: Boolean = false,
                          scriptName: String = "",
                          timeField: FieldExpr = null,
                          charts: Seq[Chart] = Seq.empty,
                          timeFormat: Option[String] = None) {
  def fields: Seq[FieldExpr] = {
    val plotFields = for (chart <- charts; s <- chart.series) yield s
    val discriminatorFields = for (chart <- charts; Series(_, _, plotType: DiscriminatedPoints) <- chart.series) yield plotType
    Seq(timeField) ++ (plotFields ++ discriminatorFields).distinct.sortBy(_.columnNumber).map(_.field)
  }
}

object GnuplotScriptWriter {

  val ToSecondPrecision = """time\(HH:[m0]{2}:[s0]{2}\)""".r
  val ToMinutePrecision = """time\(HH:[m0]{2}\)""".r
  val DateTimeToSecondPrecision = """time\(yyyy-MM-dd HH:[m0]{2}:[s0]{2}\)""".r
  val DateTimeToMinutePrecision = """time\(yyyy-MM-dd HH:[m0]{2}\)""".r

  def write(csvFileName: String, xRange: (String, String), plotOptions: GnuplotOptions): String = {
    val gnuplotTimeFormat = toGnuPlotTimeFormat(plotOptions.timeField.field)
    val displayTimeFormat = plotOptions.timeFormat.map(toGnuPlotTimeFormat).getOrElse(gnuplotTimeFormat)
    val (firstTime, lastTime) = xRange

    // using tab delimited data fails on empty cells: \t\t gets merged
    // line types cheat sheet: http://kunak.phsx.ku.edu/~sergei/Gnuplot/line_point_types.html

    val body = (for (chart <- plotOptions.charts) yield plot(chart)).mkString("\n")

    val height = 960 * math.max(1, plotOptions.charts.size / 6)
    val plotFileName = plotOptions.scriptName

    val header = s"""#!/usr/bin/env gnuplot
                     |name = '$plotFileName'
                     |csv = name.'.csv'
                     |set datafile separator ','
                     |set terminal svg enhanced mouse standalone size 1280,$height; set output name.'.svg'
                     |#set terminal pngcairo size 1280,$height; set output name.'.png'
                     |set xdata time
                     |set timefmt '$gnuplotTimeFormat'
                     |set format x '$displayTimeFormat'
                     |set format y '%.1s %c'
                     |set lmargin 10
                     |set xrange ['$firstTime':'$lastTime']
                     |set multiplot layout ${plotOptions.charts.size},1 title '$plotFileName'
                     |
                     |""".stripMargin

    header + body
  }

  private def plot(chart: Chart): String = {
    val plotsInChart = for {
      Series(field, col, plotType) <- chart.series
    } yield plotSeries(col, plotType)
    plotsInChart.mkString("plot ", ",\\\n", "\n")
  }

  private def plotSeries(col: Int, plotType: PlotType): String = {
    plotType match {
      case LinesPlot =>
        s"csv using 1:$col w lines lt $col t column($col)"
      case PointsPlot =>
        val points = s"""points lc $col pt $col ps"""
        s"""csv using 1:$col w $points 0.2 notitle,\\
           |     ''  using 1:(1/0) w $points 1 t column($col)""".stripMargin
      case DiscriminatedPoints(expr, pointTypeCol) =>
        val types = expr.pairs
        val points = s"points lc t pt t ps"
        val wordList = types.map(p => s"'${p._1}'").mkString(" ")
        s"""for [t=1:${types.size}] csv using 1:($$$pointTypeCol == t ? $$$col : 1/0) w $points 0.5 notitle,\\
           |     for [t=1:${types.size}] 1/0 w $points 1 t word("$wordList",t)""".stripMargin
    }
  }

  @tailrec
  private def toGnuPlotTimeFormat(field: String): String = {
    field match {
      case "time" | ToSecondPrecision() => "%H:%M:%S"
      case ToMinutePrecision() => "%H:%M"
      case "date" => "%Y-%m-%d"
      case "datetime" | DateTimeToSecondPrecision() => "%Y-%m-%d %H:%M:%S"
      case DateTimeToMinutePrecision() => "%Y-%m-%d %H:%M"
      case MsgPairFieldAccess(_, expr) => toGnuPlotTimeFormat(expr)
    }
  }
}