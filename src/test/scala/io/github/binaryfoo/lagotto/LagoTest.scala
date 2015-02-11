package io.github.binaryfoo.lagotto

import java.io.ByteArrayInputStream
import java.text.SimpleDateFormat
import java.util.{GregorianCalendar, TimeZone}

import com.typesafe.config.{ConfigValueFactory, ConfigFactory}
import io.github.binaryfoo.lagotto.dictionary.RootDataDictionary
import io.github.binaryfoo.lagotto.reader.ProgressInputStream
import org.joda.time.DateTimeZone
import org.scalatest.{FlatSpec, Matchers}

class LagoTest

  extends FlatSpec with Matchers with TestInput {

  val SYSTEM_TZ_ID = new SimpleDateFormat("zzz").format(new GregorianCalendar().getTime)
  // Unfortunately this will be Australian EST (AEST) or US EST (New_York) depending on the TZ of the machine running the build
  val EST_TZ = jodaTZFromJavaId("EST")
  // This ID seems to behave the same whether the current TZ is Australia or UTC
  val UYST_TZ = jodaTZFromJavaId("UYST")

  def jodaTZFromJavaId(javaUtilId: String) = {
    DateTimeZone.forTimeZone(lookupJUTimeZone(javaUtilId))
  }

  // This insanity is because TimeZone.getTimeZone() seems to recognize a different set of IDs
  def lookupJUTimeZone(javaUtilId: String): TimeZone = {
    val format = new SimpleDateFormat("zzz")
    format.parse(javaUtilId)
    format.getCalendar.getTimeZone
  }

  def iteratorOver[T](e: T*): Iterator[T] = List(e :_*).iterator

  def namedInputStreamFrom(s: String*)(sourceName: String = "") = new ProgressInputStream(new ByteArrayInputStream(s.mkString("\n").getBytes), NullProgressMeter, sourceName)

  def inputStreamFrom(s: String*) = namedInputStreamFrom(s :_*)("")

  def configWithTestDictionary = ConfigFactory.load().withValue("custom.dictionaries.dir", ConfigValueFactory.fromAnyRef("src/test/resources/"))

  val parserWithRootDictionary = new FieldExprParser(Some(RootDataDictionary()))
  val fieldParser = new FieldExprParser()
}
