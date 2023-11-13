package io.simplifier.pluginbase.util.json

import java.nio.charset.Charset
import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime}
import java.util.concurrent.TimeUnit
import akka.http.scaladsl.model.Uri
import akka.util.ByteString
import com.fasterxml.jackson.core.json.JsonReadFeature
import io.simplifier.pluginbase.util.json.JSONFormatter._
import io.simplifier.pluginbase.util.logging.ExceptionFormatting
import io.simplifier.pluginbase.util.xcoder.ByteArrayEncoding.ByteArrayEncoder
import org.json4s.ParserUtil.ParseException
import org.joda.time.format.ISODateTimeFormat
import org.joda.time.{DateTime, LocalDate}
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.TimeUnit
import scala.reflect.runtime.universe._
import scala.util.{Failure, Try}
import org.json4s._
import org.json4s.JValue
import org.json4s.prefs.EmptyValueStrategy


trait SimplifierFormats {
  implicit val formats: Formats = SimplifierFormats.formats
    // don't call apply with zero arguments
    .withPre36DeserializationBehavior

  org.json4s.jackson.JsonMethods.mapper.configure(JsonReadFeature.ALLOW_TRAILING_COMMA.mappedFeature(), true)
  /**
    * false is also the default value -
    * we prefer having an Exception than havin additional null values (which would be hard to figure out in an error case)
    * the liftweb.json behaviour (no error and no additional null value) can't be configured in json4s
    */
  org.json4s.jackson.JsonMethods.mapper.configure(JsonReadFeature.ALLOW_MISSING_VALUES.mappedFeature(), false)


  def createFormats(serializers: Seq[CustomSerializer[_]]): Formats = SimplifierFormats.createFormats(serializers)
}


//TODO (json4s) Schreibweise vereinheitlichen => groß
/**
 * JSON formats with Simplifier-specific Date/DateTime processing.
 *
 * @author Christian Simon
 */
object SimplifierFormats {

  import io.simplifier.pluginbase.util.xcoder.ByteArrayEncoding.ByteArrayEncoder

  private[this] def Logger: Logger = LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))

  /**
   * JSON Serializer for [[akka.http.scaladsl.model.Uri]].
   */
  val uriSerializer: CustomSerializer[Uri] = new CustomSerializer[Uri](_ => {
    def isValidIpV4Value(part: BigInt): Boolean = part >= 0 && part <= 255

    val deserializer: PartialFunction[JValue, Uri] = {
      case JNothing | JNull => Uri()
      case JArray(JInt(a) :: JInt(b) :: JInt(c) :: JInt(d) :: Nil) if isValidIpV4Value(a) && isValidIpV4Value(b) && isValidIpV4Value(c) && isValidIpV4Value(d) => Uri(s"$a.$b.$c.$d")
      case JString(s) => Try(Uri(s))
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the creation of an uri from the provided string: [$s].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)
    }
    val serializer: PartialFunction[Any, JValue] = {
      case uri: Uri => JString(uri.toString())
    }
    (deserializer, serializer)
  })

  /**
   * JSON Serializer for [[ByteString]].
   */
  val ByteStringSerializer: CustomSerializer[ByteString] = new CustomSerializer[ByteString](_ => {
    val deserializer: PartialFunction[JValue, ByteString] = {
      case JArray(values) => ByteArrayEncoder.Hex.encode(values.toArray).flatMap(_.toByteString)
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the parsing of a potential ByteString from the provided array.", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)
      case JNull | JNothing => ByteString()
      case other => ByteArrayEncoder.Hex.encode(other).flatMap(_.toByteString)
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the parsing of a potential ByteString " +
            s"from the provided ${ExceptionFormatting.getClassNameSafely(other, ExceptionFormatting.SimpleClassNames)}: [${renderJSONCompact(other)}].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)
    }
    val serializer: PartialFunction[Any, JValue] = {
      case byteString: ByteString => ByteArrayEncoder.Hex.encode(byteString.toArray[Byte]).map(_.toJson).getOrElse(JNothing)
    }
    (deserializer, serializer)
  })

  val smallByteStringSerializer: CustomSerializer[ByteString] = new CustomSerializer[ByteString](_ => {
    val deserializer: PartialFunction[JValue, ByteString] = {
      case JArray(Nil) => ByteString.empty
      case array@JArray(JInt(_) :: _) =>
        implicit val formats: Formats = DefaultFormats
        ByteString(array.extract[Array[Byte]])
    }
    val serializer: PartialFunction[Any, JValue] = {
      case str: ByteString =>
        implicit val formats: Formats = DefaultFormats
        Extraction.decompose(str.toArray)
    }
    (deserializer, serializer)
  })

  /**
   * JSON Serializer for [[TimeUnit]].
   */
  val TimeUnitSerializer: CustomSerializer[TimeUnit] = new CustomSerializer[TimeUnit](_ => {
    val deserializer: PartialFunction[JValue, TimeUnit] = {
      case JString(s) => scala.util.Try(TimeUnit.valueOf(s))
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the parsing of a potential time unit from the provided string: [$s].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)
      case _ => TimeUnit.SECONDS
    }
    val serializer: PartialFunction[Any, JValue] = {
      case timeUnit: TimeUnit => JString(timeUnit.name())
    }
    (deserializer, serializer)
  })

  /**
   * JSON Serializer for [[DateTime]] in ISO-8601 format.
   */
  val dateTimeSerializer: CustomSerializer[DateTime] = new CustomSerializer[DateTime](_ => {
    val deserializer: PartialFunction[JValue, DateTime] = {
      case JString(s) => Try(DateTime.parse(s, ISODateTimeFormat.dateTimeParser.withOffsetParsed))
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the parsing of a potential ISO-8601 Date Time from the provided string: [$s].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)
      case JInt(i) => new DateTime(i.longValue())
      case JDouble(d) => new DateTime(d.longValue())
    }
    val serializer: PartialFunction[Any, JValue] = {
      case dateTime: DateTime => JString(dateTime.toString(ISODateTimeFormat.dateTime))
    }
    (deserializer, serializer)
  })

  /**
   * JSON Serializer for [[LocalDateTime]] in ISO-8601 format.
   */
  val localDateTimeSerializer: CustomSerializer[LocalDateTime] = new CustomSerializer[LocalDateTime](_ => {
    val deserializer: PartialFunction[JValue, LocalDateTime] = {
      case JString(s) => Try(LocalDateTime.parse(s, DateTimeFormatter.ISO_DATE_TIME))
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the parsing of a potential ISO-8601 Local Date Time from the provided string: [$s].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)
      case JInt(i) => LocalDateTime.from(Instant.ofEpochMilli(i.longValue()))
      case JDouble(d) => LocalDateTime.from(Instant.ofEpochMilli(d.longValue()))
    }
    val serializer: PartialFunction[Any, JValue] = {
      case dateTime: LocalDateTime => JString(DateTimeFormatter.ISO_DATE_TIME.format(dateTime))
    }
    (deserializer, serializer)
  })

  /**
   * JSON Serializer for [[java.nio.charset.Charset]].
   */
  val charsetSerializer: CustomSerializer[Charset] = new CustomSerializer[Charset](_ => {
    val deserializer: PartialFunction[JValue, Charset] = {
      case JString(s) if Charset.isSupported(s) => scala.util.Try(Charset.forName(s))
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the creation of a charset for the provided charset: [$s].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)
      case JString(s) => throw logAndReturnError(new MappingException(s"The provided charset with the name: [$s] is not provided by this platform.", null))
    }
    val serializer: PartialFunction[Any, JValue] = {
      case charset: Charset => JString(charset.name())
    }
    (deserializer, serializer)
  })

  /**
   * JSON Serializer for [[LocalDate]] in ISO-8601 format.
   */
  val dateSerializer: CustomSerializer[LocalDate] = new CustomSerializer[LocalDate](_ => {
    val deserializer: PartialFunction[JValue, LocalDate] = {
      case JString(s) => Try(LocalDate.parse(s, ISODateTimeFormat.localDateParser))
        .recoverWith {
          case e => Failure(new MappingException(s"An error occurred during the parsing of a potential ISO-8601 Local Date from the provided string: [$s].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), res => res)

      case JInt(i) => new LocalDate(i.longValue())
      case JDouble(d) => new LocalDate(d.longValue())
    }
    val serializer: PartialFunction[Any, JValue] = {
      case localDate: LocalDate => JString(localDate.toString(ISODateTimeFormat.date))
    }
    (deserializer, serializer)
  })

  val byteArraySerializer: CustomSerializer[Array[Byte]] = new CustomSerializer[Array[Byte]](_ => {
    val deserializer: PartialFunction[JValue, Array[Byte]] = {
      case arr: JArray if ByteArrayEncoder.Signed.encode(arr).isSuccess => ByteArrayEncoder.Signed.encode(arr).map(_.value)
        .recoverWith {
          case e => Failure(new ParseException(s"Cannot parse: [$arr] into Array[Byte].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), arr => arr)
      case arr: JArray if ByteArrayEncoder.Signed.encode(arr).isFailure => ByteArrayEncoder.Signed.encode(arr)
        .recoverWith {
          case e => Failure(new ParseException(s"Cannot parse: [$arr] into Array[Byte].", new Exception(e)))
        }.fold(e => throw logAndReturnError(e), _ => Array.empty[Byte])
      case other => throw logAndReturnError(new ParseException(s"Cannot parse: [$other] to Array[Byte].", null))
    }
    val serializer: PartialFunction[Any, JValue] = {
      case byteArray: Array[Byte] => JArray(byteArray.map(byte => JInt(BigInt(byte))).toList)
    }
    (deserializer, serializer)
  })

  /**
   * JSON Serializer for [[Either[A,B]].
   */
  def eitherSerializer[A: Manifest : TypeTag, B: Manifest : TypeTag](implicit formats: Formats) = new CustomSerializer[Either[A, B]](_ => {
    val deserializer: PartialFunction[JValue, Either[A, B]] = {
      case v: JValue if v.extractOpt[A].nonEmpty => Left(v.extract[A])
      case v: JValue if v.extractOpt[B].nonEmpty => Right(v.extract[B])
    }
    val serializer: PartialFunction[Any, JValue] = {
      case left: Either[_, _] if left.isLeft => decomposeJSON(left.left.get, filterOuter = true)
      case right: Either[_, _] if right.isRight => decomposeJSON(right.right.get, filterOuter = true)
      case Left(left) => decomposeJSON(left, filterOuter = true)
      case Right(right) => decomposeJSON(right, filterOuter = true)
    }
    (deserializer, serializer)
  })


  private[this] def logAndReturnError(error: Throwable): Throwable = {
    if (Logger.isTraceEnabled) {
      Logger.trace(error.getMessage, error)
    } else if (Logger.isDebugEnabled) {
      Logger.debug(error.getMessage)
    }

    error
  }


  /**
   * The Empty Value Strategy is the one LiftWeb used and is more akin to what somebody expect when
   * dealing with non-existing values. Non-Existing Values will be treated as <b>JNothing</b>
   */
  private[this] val JNothingEmptyValueStrategy: EmptyValueStrategy = new EmptyValueStrategy {

    override val noneValReplacement: Option[JValue] = Some(JNothing)

    def replaceEmpty(value: JValue): JValue = value match {
      case JArray(items) => JArray(items map replaceEmpty)
      case JObject(fields) =>
        JObject(fields map { case JField(name, value) =>
          JField(name, replaceEmpty(value))
        })
      case oth => oth
    }
  }

  /**
   * Creates a new formats object with n-custom serializers.
   *
   * @param serializers own custom serializers.
   *
   * @return the default formats object
   */
  def createFormats(serializers: Seq[CustomSerializer[_]]): Formats = {
    DefaultFormats.lossless.withEmptyValueStrategy(JNothingEmptyValueStrategy) + localDateTimeSerializer + dateTimeSerializer + dateSerializer ++ serializers
  }


  val formats: Formats = DefaultFormats.lossless.withEmptyValueStrategy(JNothingEmptyValueStrategy) + localDateTimeSerializer + dateTimeSerializer +
    dateSerializer + smallByteStringSerializer + byteArraySerializer + charsetSerializer +
    uriSerializer + TimeUnitSerializer + NullableOption.serializer
}
