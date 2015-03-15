package io.github.binaryfoo.lagotto.shell

import com.typesafe.config.ConfigFactory
import io.github.binaryfoo.lagotto.LagoTest

class OptionsParserTest extends LagoTest {

  val config = ConfigFactory.load()

  "ANSI highlighting" should "be applied to full text output if supported" in {
    val opts = new OptionsParser(config, true).parse(Array("foo.log")).get
    opts.format shouldBe HighlightedText
  }

  it should "not be applied by default if not supported" in {
    val opts = new OptionsParser(config, false).parse(Array("foo.log")).get
    opts.format shouldBe FullText
  }

  it should "should be applied if requested with --highlight" in {
    val parser = new OptionsParser(config, false)

    val highlighted = parser.parse(Array("foo.log", "--highlight", "yes")).get
    highlighted.format shouldBe HighlightedText

    val plain = parser.parse(Array("foo.log", "--highlight", "no")).get
    plain.format shouldBe FullText
  }

  "--ui" should "Not apply ANSI highlighting to full text output" in {
    val opts = new OptionsParser(config, true).parse(Array("foo.log", "--ui")).get
    opts.format shouldBe FullText
  }
}
