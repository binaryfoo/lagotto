package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.JoinMode.JoinMode
import io.github.binaryfoo.lagotto._
import io.github.binaryfoo.lagotto.reader.LogType

import scala.annotation.tailrec

/**
 * What should main do? See Options for what each wonderful flag does.
 */
case class CmdLineOptions (inputFormat: LogType[LogEntry],
                   filters: Seq[LogFilter] = Seq(),
                   input: Seq[String] = Seq(), 
                   format: OutputFormat = FullText,
                   pair: Boolean = false,
                   header: Boolean = true,
                   beforeContext: Int = 0,
                   afterContext: Int = 0,
                   sortBy: Seq[SortKey] = Seq.empty,
                   joinOn: Option[(FieldExpr, JoinMode)] = None,
                   strict: Boolean = false,
                   progressMeter: ProgressMeter = NullProgressMeter,
                   histogramFields: Seq[FieldExpr] = Seq(),
                   gnuplotFileName: Option[String] = None,
                   liveUi: Boolean = false,
                   limit: Option[Int] = None) {
  
  def requiresDelayCalculation(): Boolean = includesDelayInFieldList() || includesDelayInFilters()
  
  def includesDelayInFieldList(): Boolean = outputFields().collectFirst {
      case e if e.contains(DelayExpr) => true
    }.isDefined
  
  def includesDelayInFilters(): Boolean = {
    filters.collectFirst {
      case FieldFilterOn(DelayExpr) => true
    }.isDefined
  }

  /**
   * Split the output fields (if output is tabular) into the a set of aggregates and a set of key fields.
   * Also include aggregates used in filter expressions in the set of aggregates to support filtering on aggregates
   * that aren't actually output.
   */
  def aggregationConfig(): AggregationSpec = {
    val aggregatesUsedInFilters = filters.flatMap {
      case FieldFilterOn(HasAggregateExpressions(exprs)) => exprs
      case _ => Seq()
    }
    AggregationSpec.fromExpressions(outputFields ++ aggregatesUsedInFilters)
  }

  def outputFields(): Seq[FieldExpr] = OutputFormat.fieldsFor(format)

  def pivot(): Option[PivotExpr] = {
    outputFields().collectFirst {
      case e: PivotExpr => e
    }
  }

}

case class SortKey(expr: FieldExpr, ascending: Boolean, asNumber: Boolean = true) {

  def compare(x: LogEntry, y: LogEntry): Int = {
    val r = if (asNumber) {
      expr(x).deNull("0").toInt compare expr(y).deNull("0").toInt
    } else {
      expr(x).deNull() compare expr(y).deNull()
    }
    if (ascending) r else -r
  }

}

class SortKeyOrdering(val keys: List[SortKey]) extends Ordering[LogEntry] {
  override def compare(x: LogEntry, y: LogEntry): Int = compare(x, y, keys, 0)

  @tailrec
  private def compare(x: LogEntry, y: LogEntry, keys: List[SortKey], previous: Int): Int = {
    keys match {
      case Nil => previous
      case k :: tail =>
        val current = k.compare(x, y)
        if (current != 0) current
        else compare(x, y, tail, current)
    }
  }
}
