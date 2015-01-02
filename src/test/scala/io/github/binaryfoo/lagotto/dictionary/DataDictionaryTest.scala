package io.github.binaryfoo.lagotto.dictionary

import io.github.binaryfoo.lagotto.dictionary.DataDictionary.{englishNameOf, exportNameOf, shortNameOf, typeOf}
import io.github.binaryfoo.lagotto.{LagoTest, LogEntry}

class DataDictionaryTest extends LagoTest {

  val logEntry = LogEntry()

  "English name" should "use global field from .conf" in {
    englishNameOf("2", logEntry) shouldBe Some("Primary account number")
  }

  it should "load subfields from .conf" in {
    englishNameOf("43.1", logEntry) shouldBe Some("Address")
  }

  it should "return None when unknown" in {
    englishNameOf("129", logEntry) shouldBe None
  }

  it should "fall back to shortNames" in {
    englishNameOf("0", logEntry) shouldBe Some("mti")
  }

  "Short name" should "use shortNames from .conf" in {
    shortNameOf("0", logEntry) shouldBe Some("mti")
  }

  "Export name" should "prefer entry from shortNames" in {
    exportNameOf("0", logEntry) shouldBe "mti"
    exportNameOf("2", logEntry) shouldBe "pan"
  }

  it should "fall back to camel cased english name" in {
    exportNameOf("3", logEntry) shouldBe "processingCode"
    exportNameOf("48", logEntry) shouldBe "additionalDataPrivate"
  }

  it should "finally just use the field path" in {
    exportNameOf("128.1", logEntry) shouldBe "128.1"
  }

  "Type of" should "use types key" in {
    typeOf("11", logEntry) shouldBe FieldType.Integer
  }

  it should "default to string" in {
    typeOf("2", logEntry) shouldBe FieldType.String
  }

}
