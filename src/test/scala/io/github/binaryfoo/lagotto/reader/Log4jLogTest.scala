package io.github.binaryfoo.lagotto.reader

import io.github.binaryfoo.lagotto.LagoTest

class Log4jLogTest extends LagoTest {

  "log4j record" should "not consume a following jpos record" in {
    val iterator = lineIteratorFrom( """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful
                                       |<log realm="some.channel/10.0.0.1:4321" at="Mon Nov 24 00:00:02 EST 2014.292" lifespan="10ms">
                                       |  Whinge - Exclude Me
                                       |</log>
                                       | """.stripMargin)
    Log4jLog(iterator).lines shouldBe "[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Did something useful"
  }

  it should "include two lines in multiline record" in {
    val iterator = lineIteratorFrom( """[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Line 1
                                         |Line 2
                                         |[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Next
                                         |""".stripMargin)
    Log4jLog(iterator).lines shouldBe "[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Line 1\nLine 2"
    Log4jLog(iterator).lines shouldBe "[08 Nov 2014 00:00:00,001] INFO  [a.ClassName]: Next"
  }
}
