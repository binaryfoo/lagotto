package io.github.binaryfoo.lagotto.dictionary

import java.io.File

import io.github.binaryfoo.lagotto.{LagoTest, LogEntry}

class DataDictionaryTest extends LagoTest {

  val logEntry = LogEntry()
  val dictionary = RootDataDictionary(new File("src/test/resources/"))

  import dictionary._

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

  val acmeLog = LogEntry("realm" -> "acme-terminal/127.0.0.1:4321")

  "Log entry with realm matching custom dictionary" should "pick up custom fields" in {
    englishNameOf("48.1", acmeLog) shouldBe Some("Important 48.1")
    englishNameOf("129", acmeLog) shouldBe Some("Magic 129")
    shortNameOf("48.2", acmeLog) shouldBe Some("two")
    exportNameOf("48.1", acmeLog) shouldBe "important481"
    exportNameOf("48.2", acmeLog) shouldBe "two"
    typeOf("258", acmeLog) shouldBe FieldType.Integer
  }

  it should "fall back to default for all lookups" in {
    englishNameOf("2", acmeLog) shouldBe Some("Primary account number")
    shortNameOf("2", acmeLog) shouldBe Some("pan")
    exportNameOf("2", acmeLog) shouldBe "pan"
    typeOf("11", acmeLog) shouldBe FieldType.Integer
  }

}
