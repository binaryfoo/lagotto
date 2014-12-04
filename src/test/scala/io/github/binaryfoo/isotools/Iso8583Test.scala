package io.github.binaryfoo.isotools

import io.github.binaryfoo.isotools.Iso8583.normaliseToRequestMTI
import org.scalatest.{FlatSpec, Matchers}

class Iso8583Test extends FlatSpec with Matchers {

  "MTI matcher" should "convert response MTIs" in {
    normaliseToRequestMTI("0210") shouldEqual "0200"
    normaliseToRequestMTI("0810") shouldEqual "0800"
    normaliseToRequestMTI("0830") shouldEqual "0820"
    normaliseToRequestMTI("2231") shouldEqual "2221"
  }

  it should "leave request MTIs alone" in {
    normaliseToRequestMTI("0200") shouldEqual "0200"
    normaliseToRequestMTI("0820") shouldEqual "0820"
  }

  it should "ignore invalid values" in {
    normaliseToRequestMTI("") shouldEqual ""
    normaliseToRequestMTI("020") shouldEqual "020"
    normaliseToRequestMTI("02300") shouldEqual "02300"
  }
}
