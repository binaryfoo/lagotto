package io.github.binaryfoo.lagotto

import io.github.binaryfoo.lagotto.Iso8583.{invertMTI, isRequestMTI, isResponseMTI}
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

  it should "match request MTIs" in {
    isRequestMTI("0200") shouldBe true
    isRequestMTI("0210") shouldBe false
  }

  it should "match response MTIs" in {
    isResponseMTI("0210") shouldBe true
    isResponseMTI("0200") shouldBe false
  }

  "2 character HSM MTIs" should "be handled" in {
    invertMTI("C2") shouldEqual "C3"
    invertMTI("C3") shouldEqual "C2"
    invertMTI("BE") shouldEqual "BF"
    invertMTI("BF") shouldEqual "BE"

    isResponseMTI("BF") shouldBe true
    isResponseMTI("BE") shouldBe false
  }
}
