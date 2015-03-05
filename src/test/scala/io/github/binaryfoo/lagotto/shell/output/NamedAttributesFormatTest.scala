package io.github.binaryfoo.lagotto.shell.output

import io.github.binaryfoo.lagotto.{SimpleLogEntry, JposEntry, LagoTest}
import io.github.binaryfoo.lagotto.dictionary.RootDataDictionary

class NamedAttributesFormatTest extends LagoTest {

  val format = NamedAttributesFormat(RootDataDictionary())

  "A name attribute" should "be added to each field" in {
    val out = format.apply(JposEntry(lines = """<isomsg>
                                               |  <field id="7" value="1124000003"/>
                                               |  <isomsg id="43">
                                               |    <field id="3" value="QLD"/>
                                               |  </isomsg>
                                               |  <field id="999" value="831"/>
                                               |</isomsg>""".stripMargin)).get
    out should include("""<field id="7" name="Transmission date and time" value="1124000003"/>""")
    out should include("""<field id="3" name="State" value="QLD"/>""")
    out should include("""<field id="999" value="831"/>""")
  }

  it should "handle no lines in message" in {
    format.apply(JposEntry()) shouldBe Some("")
  }

  "A simple log entry" should "pass through unchanged" in {
    val entry = SimpleLogEntry(lines = "line 1\nline 2")
    format.apply(entry).get shouldBe "line 1\nline 2"
  }
}
