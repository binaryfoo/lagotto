package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.LagoTest
import org.joda.time.DateTime

class CsvLogTest extends LagoTest {

  "Csv log" should "parse fields from header" in {
    val iterator = iteratorOver("""field1,field2
                                  |val1.1,val1.2
                                  |val2.1,val2.2
                                  |""".stripMargin)

    CsvLog.apply(iterator).toSeq("field1", "field2") shouldBe Seq("val1.1", "val1.2")
    CsvLog.apply(iterator).toSeq("field1", "field2") shouldBe Seq("val2.1", "val2.2")
    CsvLog.apply(iterator) shouldBe null
  }

  it should "return an empty iterator with only a header row" in {
    CsvLog.apply(iteratorOver("field1,field2")) shouldBe null
  }

  it should "parse datetime if present" in {
    val iterator = iteratorOver("""datetime,field2
                                  |2015-01-17 16:12:18.483
                                  |""".stripMargin)

    CsvLog.apply(iterator).timestamp shouldBe new DateTime(2015, 1, 17, 16, 12, 18, 483)
  }

  private def iteratorOver(text: String) = new SourceLineIterator(text.split('\n').iterator, "testSource", true, true)
}
