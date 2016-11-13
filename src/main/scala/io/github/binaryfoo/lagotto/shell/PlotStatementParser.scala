package io.github.binaryfoo.lagotto.shell

import fastparse.all._
import fastparse.core.{Mutable, ParseCtx}
import io.github.binaryfoo.lagotto.output._
import io.github.binaryfoo.lagotto.{FieldExpr, FieldExprParser, OrdinalExpr, TimeExpr}

import scala.annotation.tailrec
import scala.collection.mutable

object PlotField extends fastparse.all.Parser[String] {

  @tailrec
  private def findFieldEnd(input: ParserInput, pos: Int = 0, depth: Int = 0): Int = {
    if (pos >= input.length) {
      -pos
    } else {
      input(pos) match {
        case ',' | ')' if depth == 0 => if (pos == 0) 0 else pos
        case '(' => findFieldEnd(input, pos + 1, depth + 1)
        case ')' => findFieldEnd(input, pos + 1, depth - 1)
        case _ => findFieldEnd(input, pos + 1, depth)
      }
    }
  }

  def parseRec(cfg: ParseCtx[Char, String], index: Int): Mutable[String, Char, String] = {
    val input = cfg.input
    if (index >= input.length) fail(cfg.failure, index)
    val end = findFieldEnd(input, index)
    if (end > 0) success(cfg.success, input.slice(index, end), end, Set.empty, false)
    else fail(cfg.failure, -end)
  }
}

class PlotStatementParser(val fieldExprParser: FieldExprParser) {

  private val plotType = (StringIn("points", "lines").! ~ (" labelled by (" ~ PlotField ~ ")").?).map {
    case ("points", None) => PointsPlot
    case ("points", Some(field)) =>
      val expr = fieldExprParser.stringAsFieldExpr(field)
      val ordinal = OrdinalExpr("ordinal(" + expr.field + ")", expr)
      DiscriminatedPoints(ordinal, 0)
    case ("lines", None) => LinesPlot
  }
  private val chart = "(" ~ PlotField.rep(1, sep = ",") ~ ")" ~ (" as " ~ plotType).?
  private val stmt = (AnyChar ~ !"vs ").rep(1).! ~ " vs " ~ chart.rep(1, sep = "," ~ " ".rep)

  def parse(plot: String): (FieldExpr, Seq[Chart]) = {
    val Parsed.Success((time, setOfSeries), _) = stmt.parse(plot)
    val timeExpr = TimeExpr.unapply(time).get
    val columnNumbers = mutable.Map[FieldExpr, Int]()
    val charts = for {
      (set, plotType) <- setOfSeries
      chart = toChart(set, columnNumbers, plotType.getOrElse(LinesPlot))
    } yield chart
    (timeExpr, charts)
  }

  import PlotStatementParser.ColumnNumbers

  private def toChart(fields: Seq[String], columnNumbers: mutable.Map[FieldExpr, Int], plotType: PlotType): Chart = {
    val numberedPlotType = plotType match {
      case d: DiscriminatedPoints =>
        val columnNumber = columnNumbers.numberFor(d.field)
        val valueCollector = (for (f <- columnNumbers.keys if f == d.field) yield f.asInstanceOf[OrdinalExpr]).head
        d.copy(columnNumber = columnNumber, field = valueCollector)
      case t => t
    }
    val series = for {
      field <- fields
      expr = fieldExprParser.stringAsFieldExpr(field)
      col = columnNumbers.numberFor(expr)
    } yield Series(expr, col, numberedPlotType)
    Chart(series)
  }

}

object PlotStatementParser {
  implicit class ColumnNumbers(val map: mutable.Map[FieldExpr, Int]) extends AnyVal {
    def numberFor(field: FieldExpr): Int = {
      map.getOrElseUpdate(field, { if (map.isEmpty) 2 else map.values.max + 1 })
    }
  }
}
