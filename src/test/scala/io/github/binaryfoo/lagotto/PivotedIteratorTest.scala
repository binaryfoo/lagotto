package io.github.binaryfoo.lagotto

class PivotedIteratorTest extends LagoTest {

  import fieldParser.FieldExpr.expressionFor

  val rotateOn = fieldParser.DirectExpr.unapply("at").get
  val count = expressionFor("n")
  val entries = Seq(
    SimpleLogEntry("at" -> "15:59", "mti" -> "0200", "n" -> "10"),
    SimpleLogEntry("at" -> "15:59", "mti" -> "0210", "n" -> "9"),
    SimpleLogEntry("at" -> "16:00", "mti" -> "0200", "n" -> "5")
  )
  val pivotExpr = PivotExpr("mti", expressionFor("mti"))
  entries.foreach(pivotExpr)

  "Pivot iterator" should "output one row per (rotateOn, pivotExpr) pair" in {
    val iterator = new PivotedIterator(rotateOn, pivotExpr, Seq(count), entries.toIterator)
    val result = iterator.toList.map(_.exportAsSeq)
    result shouldBe List(
      Seq("at" -> "15:59", "0200 - n" -> "10", "0210 - n" -> "9"),
      Seq("at" -> "16:00", "0200 - n" -> "5",  "0210 - n" -> "0")
    )
  }

  "Aggregate of pivot" should "apply to all columns of a single pivoted row" in {
    val sumOfPivotedCounts = expressionFor("sum(pivoted(n))")
    val iterator = new PivotedIterator(rotateOn, pivotExpr, Seq(count, sumOfPivotedCounts), entries.toIterator)
    val result = iterator.toList.map(_.exportAsSeq)
    result shouldBe List(
      Seq("at" -> "15:59", "0200 - n" -> "10", "0210 - n" -> "9", "sum(pivoted(n))" -> "19"),
      Seq("at" -> "16:00", "0200 - n" -> "5",  "0210 - n" -> "0", "sum(pivoted(n))" -> "5")
    )
  }

  // TODO: work in progress
//  "Conditional count of pivot" should "apply to pivot result" in {
//    val sumOfPivotedCounts = expressionFor("count(if(pivoted(n)>1))")
//    val iterator = new PivotedIterator(rotateOn, pivotExpr, Seq(count, sumOfPivotedCounts), entries.toIterator)
//    val result = iterator.toList.map(_.exportAsSeq)
//    result shouldBe List(
//      Seq("at" -> "15:59", "0200 - n" -> "10", "0210 - n" -> "9", "count(if(pivoted(n)>1))" -> "2"),
//      Seq("at" -> "16:00", "0200 - n" -> "5",  "0210 - n" -> "0", "count(if(pivoted(n)>1))" -> "1")
//    )
//  }
}
