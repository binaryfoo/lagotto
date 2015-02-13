package io.github.binaryfoo.lagotto.shell

import io.github.binaryfoo.lagotto.JoinMode.JoinMode
import io.github.binaryfoo.lagotto._

/**
 * What should main do? See Options for what each wonderful flag does.
 */
case class CmdLineOptions (filters: Seq[LogFilter] = Seq(),
                   input: Seq[String] = Seq(), 
                   inputFormat: Option[String] = None,
                   format: OutputFormat = FullText,
                   pair: Boolean = false,
                   header: Boolean = true,
                   beforeContext: Int = 0,
                   afterContext: Int = 0,
                   sortBy: Option[FieldExpr] = None,
                   joinOn: Option[(FieldExpr, JoinMode)] = None,
                   sortDescending: Boolean = false,
                   strict: Boolean = false,
                   progressMeter: ProgressMeter = NullProgressMeter,
                   histogramFields: Seq[FieldExpr] = Seq(),
                   gnuplotFileName: Option[String] = None,
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
