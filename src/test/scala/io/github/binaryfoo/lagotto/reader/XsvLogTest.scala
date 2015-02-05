package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.LagoTest
import org.joda.time.DateTime

class XsvLogTest extends LagoTest {

  private val csvLog = new XsvLog()

  "Csv log" should "parse fields from header" in {
    val iterator = iteratorOver("""field1,field2
                                  |val1.1,val1.2
                                  |val2.1,val2.2
                                  |""".stripMargin)

    csvLog.apply(iterator).toSeq("field1", "field2") shouldBe Seq("val1.1", "val1.2")
    csvLog.apply(iterator).toSeq("field1", "field2") shouldBe Seq("val2.1", "val2.2")
    csvLog.apply(iterator) shouldBe null
  }

  it should "return an empty iterator with only a header row" in {
    csvLog.apply(iteratorOver("field1,field2")) shouldBe null
  }

  it should "parse datetime if present" in {
    val iterator = iteratorOver("""datetime,field2
                                  |2015-01-17 16:12:18.483
                                  |""".stripMargin)

    csvLog.apply(iterator).timestamp shouldBe new DateTime(2015, 1, 17, 16, 12, 18, 483)
  }

  private def iteratorOver(text: String) = new SourceLineIterator(text.split('\n').iterator, "testSource", true, true)
}
