package io.simplifier.pluginbase.util.json

import akka.http.scaladsl.model.Uri
import akka.util.{ByteString, CompactByteString}
import io.simplifier.pluginbase.util.json.SimplifierFormats.ByteStringSerializer
import io.simplifier.pluginbase.util.json.SimplifierFormatsTest._
import org.json4s.JsonAST.JString
import org.json4s.{CustomSerializer, DefaultFormats, Extraction, Formats, JField, JNothing, JObject}
import org.json4s.jackson.Serialization._
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, LocalDate}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers.{a, convertToAnyMustWrapper, equal}

import java.nio.charset.Charset
import java.time.LocalDateTime
import java.util.Date
import java.util.concurrent.TimeUnit
import scala.reflect.runtime.universe.typeTag

/**
 * test added before migrating from liftweb.json to json4s
 */
class SimplifierFormatsTest extends AnyFunSuite with SimplifierFormats {

  test("option of empty map") {
    import org.json4s.JsonDSL._
    val o: JObject = ("xyz" -> "None") ~ ("abc" -> ("one" -> "v1") ~ ("two" -> "v2"))

    val optFull1: Option[Map[String, String]] = (o \ "abc").extractOpt
    optFull1 mustBe None // strange

    val optFull2: Option[Map[String, String]] = (o \ "abc").toOption.map(_.extract[Map[String, String]])
    optFull2 mustBe Some(Map(("one" -> "v1"), ("two" -> "v2")))

    val optFull: Option[Map[String, String]] = (o \ "abc").extractOpt[Map[String, String]]
    optFull mustBe Some(Map(("one" -> "v1"), ("two" -> "v2")))

    val optEmpty: Option[Map[String, String]] = (o \ "def").extractOpt
    optEmpty mustBe None

    val optEmpty2: Option[Map[String, String]] = (o \ "def").toOption.map(_.extract[Map[String, String]])
    optEmpty2 mustBe None

    val optEmpty1: Option[Map[String, String]] = (o \ "def").extractOpt[Map[String, String]]
    optEmpty1 mustBe Some(Map()) // strange
  }

  test("option of empty map in case class") { // so inside the object hierarchy the option handling is fine - only using extractOpt directly has behavior giving Some(Map())
    import org.json4s.JsonDSL._
    val full: JObject = ("mapOpt" -> ("key" -> "value") ~ ("key" -> "otherValue")) ~ ("xyz" -> None.asInstanceOf[Option[String]])
    val empty: JObject = ("mapOpt" -> None.asInstanceOf[Option[String]])

    val fullValue = full.extract[OptTest1]
    fullValue mustBe OptTest1(Some(Map(("key" -> "value"), ("key" -> "otherValue"))))

    val emptyValue = empty.extract[OptTest1]
    emptyValue mustBe OptTest1(None)
  }

  test("option of case class") { // and extractOpt also works unexpected, with options of other classes (and not only options of maps/lists,...)
    import org.json4s.JsonDSL._
    val full: JObject = ("mapOpt" -> ("key" -> "value") ~ ("key" -> "otherValue")) ~ ("xyz" -> None.asInstanceOf[Option[String]])
    val empty: JObject = ("mapOpt" -> None.asInstanceOf[Option[String]])

    val fullValue: Option[OptTest1] = full.extractOpt[OptTest1]
    fullValue mustBe Some(OptTest1(Some(Map(("key" -> "value"), ("key" -> "otherValue")))))

    val emptyValue = empty.extractOpt[OptTest1]
    emptyValue mustBe Some(OptTest1(None)) // strange

    val emptyValue1 = empty.toOption.map(_.extract[OptTest1])
    emptyValue1 mustBe Some(OptTest1(None)) // still strange

    val emptyValue2 = JNothing.extractOpt[OptTest1]
    emptyValue2 mustBe Some(OptTest1(None)) // strange, strange, strange...

    val emptyValue3 = JNothing.toOption.map(_.extract[OptTest1])
    emptyValue3 mustBe None
  }

  test("null arg test") {
    Extraction.extractOpt[NullArgTest](JNothing) mustBe None
  }

  test("write tests") {
    write(MyDate(new Date(213123))) mustBe """{"d":"1970-01-01T00:03:33.123Z"}"""
    write(MyDateTime(new DateTime(2023, 12, 30, 13, 35))) mustBe """{"d":"2023-12-30T13:35:00.000+01:00"}"""
    write(MyLocalDateTime(LocalDateTime.of(2023, 12, 30, 13, 35, 51))) mustBe """{"d":"2023-12-30T13:35:51"}"""

    read[MyDate]("""{"d":"1970-01-01T00:03:33.123Z"}""") mustBe MyDate(new Date(213123))

    val dateTime = read[MyDateTime]("""{"d":"2023-12-30T13:35:00.000+01:00"}""")
    dateTime mustBe a[MyDateTime]
    // timezone might differ (once Berlin, once +1)
    dateTime.d.getMillis must equal(new DateTime(2023, 12, 30, 13, 35).getMillis)

    read[MyLocalDateTime]("""{"d":"2023-12-30T14:35:51"}""") must equal(MyLocalDateTime(LocalDateTime.of(2023, 12, 30, 14, 35, 51)))
  }

  test("eitherSerialization") {
    val automaticCertificateDataSerializer: CustomSerializer[Either[String, Array[Byte]]]
    = SimplifierFormats.eitherSerializer(manifest[String], typeTag[String], manifest[Array[Byte]], typeTag[Array[Byte]], SimplifierFormats.formats)
    implicit val formats: Formats = SimplifierFormats.formats + automaticCertificateDataSerializer

    var testEither: Either[String, Array[Byte]] = Left("abc")
    write(testEither) mustBe "\"abc\""
    read[Either[String, Array[Byte]]]("\"abc\"") mustBe Left("abc")

    testEither = Right("def".getBytes("UTF-8"))
    write(testEither) mustBe "[100,101,102]"
    val result: Either[String, Array[Byte]] = read[Either[String, Array[Byte]]]("[100,101,102]")
    result mustBe a[Right[_, _]]
    result.right.get mustEqual "def".getBytes("UTF-8")
  }

  test("dateDeserializer") {
    val dateString = "\"" + new LocalDate(2023, 12, 30).toString(ISODateTimeFormat.date) + "\""

    var localDate: LocalDate = read[LocalDate]("1687434269074")
    localDate.toString mustBe "2023-06-22"

    localDate = read[LocalDate]("1687434269074.0")
    localDate.toString mustBe "2023-06-22"

    localDate = read[LocalDate](dateString)
    localDate.toString mustBe "2023-12-30"

  }

  test("DateTime serialization") {
    val dateString = "\"" + new DateTime(2023, 12, 30, 14, 47).toString(ISODateTimeFormat.dateTime()) + "\""

    var dateTime: DateTime = read[DateTime]("1687434269074")
    dateTime.toString mustBe "2023-06-22T13:44:29.074+02:00"

    dateTime = read[DateTime]("1687434269074.0")
    dateTime.toString mustBe "2023-06-22T13:44:29.074+02:00"

    dateTime = read[DateTime](dateString)
    dateTime.toString mustBe "2023-12-30T14:47:00.000+01:00"

  }

  test("charset serialization") {
    write(Charset.forName("UTF-8")) mustBe "\"UTF-8\""

    read[Charset]("\"UTF-8\"") mustBe Charset.forName("UTF-8")
    val exceptionUnknownCharset = intercept[Exception] {
      read[Charset]("\"UTF-8something\"") mustBe Charset.forName("UTF-8")
    }
    exceptionUnknownCharset.getMessage mustBe "The provided charset with the name: [UTF-8something] is not provided by this platform."
  }

  test("time unit serialization") {
    write(TimeUnit.SECONDS) mustBe "\"SECONDS\""

    read[TimeUnit]("\"SECONDS\"") mustBe TimeUnit.SECONDS

  }

  test("smallByteStringSerializer") {
    write(CompactByteString("abcDEF".getBytes("UTF-8"))) mustBe "[97,98,99,68,69,70]"

    read[ByteString]("[97,98,99,68,69,70]") mustEqual CompactByteString("abcDEF".getBytes("UTF-8"))

  }

  test("ByteStringSerializer") {
    implicit val formats: Formats = SimplifierFormats.formats + ByteStringSerializer

    write(CompactByteString("abcDEF".getBytes("UTF-8"))) mustBe """["61","62","63","44","45","46"]"""

    read[ByteString]("[97,98,99,68,69,70]") mustEqual CompactByteString("abcDEF".getBytes("UTF-8"))
    read[ByteString]("""["61","62","63","44","45","46"]""") mustEqual CompactByteString("abcDEF".getBytes("UTF-8"))
  }

  test("Uri Serializer") {
    val simpleUri: Uri = Uri("https://www.google.de")
    val localUri: Uri = Uri("http://localhost:8080/my/url?www=def&A=b")
    val ftpUri: Uri = Uri("ftp://123.3.12.203:8080/my/url?www=def%20a")
    val ipOnlyUri: Uri = Uri("123.3.12.203")
    write(simpleUri) mustBe "\"https://www.google.de\""
    write(localUri) mustBe "\"http://localhost:8080/my/url?www=def&A=b\""
    write(ftpUri) mustBe "\"ftp://123.3.12.203:8080/my/url?www=def%20a\""
    write(ipOnlyUri) mustBe "\"123.3.12.203\""

    read[Uri]("\"https://www.google.de\"") mustEqual simpleUri
    read[Uri]("\"http://localhost:8080/my/url?www=def&A=b\"") mustEqual localUri
    read[Uri]("\"ftp://123.3.12.203:8080/my/url?www=def%20a\"") mustEqual ftpUri
    read[Uri]("\"123.3.12.203\"") mustEqual ipOnlyUri
    read[Uri]("[123,3,12,203]") mustEqual ipOnlyUri

  }

  test("EmptyValueStrategy") {
    val string: String = "42"
    val classWithResultsNone: JObject = JObject(JField("test", JNothing))
    val classWithResultsSome: JObject = JObject(JField("test", JString(string)))
    val classWithoutResults: JObject = JObject()

    Extraction.decompose(ClassWithNone(None))(SimplifierFormats.formats) mustBe classWithResultsNone
    Extraction.decompose(ClassWithNone(Some(string)))(SimplifierFormats.formats) mustBe classWithResultsSome
    Extraction.decompose(ClassWithNone(None))(DefaultFormats.lossless) mustBe classWithoutResults

  }

}

case class OptTest1(mapOpt: Option[Map[String, String]])

case class OptTest2(mapOpt: Option[Seq[SimpleKeyValue]])

case class SeqTest1(mapOpt: Seq[SimpleKeyValue])

case class OptTest3(mapOpt: Option[SimpleKeyValue])

case class SimpleKeyValue(key: String, value: String)


// this object and the class need to be at this hierachy level
object NullArgTest {
  def apply() = new NullArgTest("abc")
}

case class NullArgTest(param: String)

object SimplifierFormatsTest {

  case class ClassWithNone(test: Option[String])

  case class MyBool(b: Boolean)

  case class MyDouble(d: Double)

  case class MyBigDec(d: BigDecimal)

  case class MyInt(d: Int)

  case class MyString(s: String)

  case class MyStrings(s1: String, s2: String)

  case class MySeq(s: Seq[String])

  case class MyOption(o: Option[String])

  case class MyDate(d: Date)

  case class MyDateTime(d: DateTime)

  case class MyLocalDateTime(d: LocalDateTime)

}
