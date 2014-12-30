package io.github.binaryfoo.lagotto

import scala.collection.immutable.ListSet

/**
 * If aggregates is empty no aggregation needs to be done. Otherwise calculate a set of aggregates for each group
 * identified by a set of key values.
 */
case class AggregationSpec(keys: Seq[FieldExpr] = Seq(), aggregates: Set[AggregateExpr] = ListSet())

object AggregationSpec {

  def fromExpressions(expressions: Seq[FieldExpr]): AggregationSpec = {
    expressions.foldLeft(AggregationSpec()) { case (current, expr) =>
      expr match {
        case HasAggregateExpressions(moreAggregates) => current.copy(aggregates = current.aggregates ++ moreAggregates)
        case e: FieldExpr => current.copy(keys = current.keys :+ e)
      }
    }
  }
}