package io.simplifier.pluginbase.util.http

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.util.ByteString
import io.simplifier.pluginbase.util.logging.Logging
import org.json4s.jackson.JsonMethods._
import org.json4s.{JValue, _}

import java.lang.reflect.InvocationTargetException


/**
 * Automatic to and from JSON marshalling/unmarshalling using an in-scope liftweb-json protocol.
 *
 * Pretty printing is enabled if an implicit [[JsonMarshalling.JsonRenderPretty]] is in scope.
 */
trait JsonMarshalling extends Logging {

  import JsonMarshalling._
  import io.simplifier.pluginbase.util.json.JSONCompatibility._

  /**
   * HTTP entity => JSON String
   *
   * @note this unmarshaller is only applicable for the content type: application/json
   * @return unmarshaller for JSON String
   */
  val jsonStringUnmarshaller: FromEntityUnmarshaller[String] = {
    Unmarshaller.byteStringUnmarshaller
      .forContentTypes(`application/json`)
      .mapWithCharset {
        case (ByteString.empty, _) => throw Unmarshaller.NoContentException
        case (data, charset) => data.decodeString(charset.nioCharset.name)
      }
  }

  /**
   * HTTP entity => <b>JValue</b>
   *
   * @note this unmarshaller is only applicable for the content type: application/json
   * @return unmarshaller for<b>JValue</b>
   */
  implicit val jsonValueUnmarshaller: FromEntityUnmarshaller[JValue] = jsonStringUnmarshaller.map(a => parseJsonOrEmptyString(a))

  /**
   * HTTP entity => `A`
   *
   * @tparam A type to decode
   * @return unmarshaller for `A`
   */
  def explicitJsonTypeUnmarshaller[A: Manifest](implicit formats: Formats): FromEntityUnmarshaller[A] = {
    jsonValueUnmarshaller
      .map { value =>
        logger.trace(JsonValueForExtractionReceived(value))
        val extractedValue: A = Extraction.extract[A](value)
        logger.trace(JsonValueExtracted(extractedValue))
        extractedValue
      }
      .recover { _ =>
        _ => {
          case e: MappingException if (e.cause != null && e.cause.isInstanceOf[InvocationTargetException]) => throw e.cause.getCause
        }
      }
  }

  /**
   * JSON-String => HTTP entity
   *
   * @return marshaller for JSON String
   */
  val jsonStringMarshaller: ToEntityMarshaller[String] = Marshaller.stringMarshaller(`application/json`)

  /**
   * `JValue` => HTTP entity
   *
   * @return marshaller for a `JValue`
   */
  implicit def jsonValueMarshaller(implicit renderSetting: JsonRenderSetting = JsonRenderCompact): ToEntityMarshaller[JValue] = {
    renderSetting match {
      case JsonRenderCompact => jsonStringMarshaller.compose(compactRenderOrNothing)
      case JsonRenderPretty => jsonStringMarshaller.compose(prettyRenderOrNothing)
    }
  }

  private def compactRenderOrNothing(json: JValue): String = json match {
    case JNothing => ""
    case _ => compact(render(json))
  }

  private def prettyRenderOrNothing(json: JValue): String = json match {
    case JNothing => ""
    case _ => pretty(render(json))
  }

  /**
   * `A` => HTTP entity
   *
   * @tparam A type to encode
   * @return marshaller for any `A` value
   */
  def explicitJsonTypeMarshaller[A <: AnyRef](implicit formats: Formats,
                                              renderSetting: JsonRenderSetting = JsonRenderCompact): ToEntityMarshaller[A] = jsonValueMarshaller.compose(Extraction.decompose)

}


/**
 * Automatic to and from JSON marshalling/unmarshalling using an in-scope liftweb-json protocol.
 *
 * Pretty printing is enabled if an implicit [[JsonMarshalling.JsonRenderPretty]] is in scope.
 */
object JsonMarshalling extends JsonMarshalling with Logging {

  sealed abstract class JsonRenderSetting

  case object JsonRenderPretty extends JsonRenderSetting

  case object JsonRenderCompact extends JsonRenderSetting

  private[JsonMarshalling] def JsonValueForExtractionReceived(value: JValue): String = s"Received Json Value to unmarshall: [${value.toString}]."

  private[JsonMarshalling] def JsonValueExtracted[T](value: T): String = s"Extracted Json Value by unmarshaller: [${value.toString}]."

}