package io.github.binaryfoo.lagotto

import scala.collection.immutable.ListSet

/**
 * If aggregates is empty no aggregation needs to be done. Otherwise calculate a set of aggregates for each group
 * identified by a set of key values.
 */
case class AggregationSpec(keys: Seq[GroundedFieldExpr] = Seq(), aggregates: Set[AggregateFieldExpr] = ListSet())

object AggregationSpec {

  def fromExpressions(expressions: Seq[GroundedFieldExpr]): AggregationSpec = {
    expressions.foldLeft(AggregationSpec()) { case (current, expr) =>
      expr match {
        case HasAggregateExpressions(moreAggregates) => current.copy(aggregates = current.aggregates ++ moreAggregates)
        case e: GroundedFieldExpr => current.copy(keys = current.keys :+ e)
      }
    }
  }
}