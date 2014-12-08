package io.github.binaryfoo.isotools

import io.github.binaryfoo.isotools.Iso8583.invertMTI
import org.scalatest.{FlatSpec, Matchers}

class Iso8583Test extends FlatSpec with Matchers {

  "MTI matcher" should "convert response MTI to request" in {
    invertMTI("0210") shouldEqual "0200"
    invertMTI("0810") shouldEqual "0800"
    invertMTI("0830") shouldEqual "0820"
    invertMTI("2231") shouldEqual "2221"
  }

  it should "convert request MTI to response" in {
    invertMTI("0200") shouldEqual "0210"
    invertMTI("0820") shouldEqual "0830"
    invertMTI("2221") shouldEqual "2231"
  }

  it should "ignore invalid values" in {
    invertMTI("") shouldEqual ""
    invertMTI("020") shouldEqual "020"
    invertMTI("02300") shouldEqual "02300"
  }
}
