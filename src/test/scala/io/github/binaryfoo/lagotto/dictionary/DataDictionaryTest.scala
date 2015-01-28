package io.github.binaryfoo.lagotto.dictionary

import java.io.File

import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import io.github.binaryfoo.lagotto.{LagoTest, JposEntry}

class DataDictionaryTest extends LagoTest {

  val logEntry = JposEntry()
  val dictionary = RootDataDictionary(configWithTestDictionary)

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
    exportNameOf("3", logEntry) shouldBe "processing_code"
    exportNameOf("48", logEntry) shouldBe "additional_data_private"
  }

  it should "finally just use the field path sanitized for SQL" in {
    exportNameOf("128.1", logEntry) shouldBe "f_128_1"
  }

  "Name of" should "honour name type" in {
    nameOf(NameType.Short, "3", logEntry) shouldBe None
    nameOf(NameType.English, "3", logEntry) shouldBe Some("Processing code")
    nameOf(NameType.Camel, "3", logEntry) shouldBe Some("processingCode")
    nameOf(NameType.Snake, "3", logEntry) shouldBe Some("processing_code")
  }

  "Type of" should "use types key" in {
    typeOf("11", logEntry) shouldBe FieldType.Integer
  }

  it should "default to string" in {
    typeOf("2", logEntry) shouldBe FieldType.String
  }

  "Translate value" should "use translations from .conf" in {
    translateValue("70", JposEntry("0" -> "0800"), "301") shouldBe Some("Echo")
  }

  it should "default to None" in {
    translateValue("70", JposEntry("0" -> "0800"), "666") shouldBe None
  }

  "Field for short name" should "use shortNames from .conf" in {
    fieldForShortName("stan", logEntry) shouldBe Some("11")
  }

  it should "match on camel cased name too" in {
    fieldForShortName("settlementAmount", logEntry) shouldBe Some("5")
  }

  it should "match on snake cased name too" in {
    fieldForShortName("settlement_amount", logEntry) shouldBe Some("5")
  }

  it should "return None if no name applies" in {
    fieldForShortName("rubbish", logEntry) shouldBe None
  }

  val acmeRealm = "acme-terminal/127.0.0.1:4321"
  val acmeLog = JposEntry("realm" -> acmeRealm)

  "Log entry with realm matching custom dictionary" should "pick up custom fields" in {
    englishNameOf("48.1", acmeLog) shouldBe Some("Important 48.1")
    englishNameOf("129", acmeLog) shouldBe Some("Magic 129")
    shortNameOf("48.2", acmeLog) shouldBe Some("two")
    exportNameOf("48.1", acmeLog) shouldBe "important_481"
    exportNameOf("48.2", acmeLog) shouldBe "two"
    typeOf("258", acmeLog) shouldBe FieldType.Integer
    translateValue("48.3", JposEntry("realm" -> acmeRealm, "0" -> "4321"), "one") shouldBe Some("Beep Beep")
    fieldForShortName("two", acmeLog) shouldBe Some("48.2")
  }

  it should "fall back to default for all lookups" in {
    englishNameOf("2", acmeLog) shouldBe Some("Primary account number")
    shortNameOf("2", acmeLog) shouldBe Some("pan")
    exportNameOf("2", acmeLog) shouldBe "pan"
    typeOf("11", acmeLog) shouldBe FieldType.Integer
    translateValue("48.3", JposEntry("realm" -> acmeRealm, "0" -> "4322"), "one") shouldBe None
    translateValue("48.3", acmeLog, "two") shouldBe None
    fieldForShortName("stan", acmeLog) shouldBe Some("11")
  }

  it should "iterate through multiple dictionaries" in {
    shortNameOf("99", JposEntry("realm" -> acmeRealm, "0" -> "9200")) shouldBe Some("hot air balloons")
  }

  "Custom dictionaries" should "be found in application.conf too" in {
    val config = ConfigFactory.load().withFallback(ConfigFactory.parseFile(new File(testFile("test-dictionary.conf"))))
    val customFromMergeDictionary = RootDataDictionary(config)
    customFromMergeDictionary.englishNameOf("48.1", acmeLog) shouldBe Some("Important 48.1")
  }
}
