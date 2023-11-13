package io.simplifier.pluginbase.util.api

import akka.http.scaladsl.marshalling.{Marshaller, PredefinedToResponseMarshallers, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCode}
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import ApiFailureResponse.ApiFailureResponseJson
import io.simplifier.pluginbase.util.http.JsonMarshalling.explicitJsonTypeUnmarshaller
import io.simplifier.pluginbase.util.json.JSONFormatter._
import io.simplifier.pluginbase.util.json.SimplifierFormats
import org.json4s._
import org.json4s.jackson.Serialization._
import org.json4s.JValue

import scala.xml.Elem

/**
  * Serializable API Failure, marshallable as response. By content negotiation the response is either
  * returned as JSON or as HTML.
  *
  * @author Christian Simon
  */
class ApiFailureResponse(val statusCode: StatusCode, details: JValue, message: Option[String],
                         val errorCode: Option[String], val errorArgs: Option[Seq[String]]) {

  val intVal: Int = statusCode.intValue
  val reason: String = statusCode.reason

  val messageText: String = message.filter(_.nonEmpty).getOrElse(statusCode.defaultMessage)

  private val htmlTitle = s"Error $intVal - $reason"

  /**
    * HTML response.
    */
  val htmlValue: Elem =
    <html>
      <title>
        {htmlTitle}
      </title>
      <h1>
        {htmlTitle}
      </h1>
      <p>
        {messageText}
      </p>{if (errorCode.isDefined) {
      <div>
        Code:
        <kbd>
          {errorCode.getOrElse("")}
        </kbd>
      </div>
    }}{if (details != JNothing) {
      <div>
        Details:
        <kbd>
          {renderJSONCompact(details)}
        </kbd>
      </div>
    }}
    </html>

  /**
    * JSON response.
    */
  val jsonValue: ApiFailureResponseJson = {

    ApiFailureResponseJson(
      message = messageText,
      errorCode = errorCode,
      errorArgs = errorArgs,
      details = details match {
        case JString(s) => Option(s).filter(_.trim.nonEmpty).map(JString).getOrElse(JNothing)
        case other => Option(renderJSONPretty(other)).filter(_.trim.nonEmpty).map(JString).getOrElse(JNothing)
      }
    )
  }

  val jsonValueWithJsonDetails = ApiFailureResponseJson(messageText, success = false, errorCode, errorArgs, details)

}

object ApiFailureResponse extends SimplifierFormats {

  import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
  import ApiMessage.jsonTypeMarshaller

  case class ApiFailureResponseJson(message: String,
                                    success: Boolean = false,
                                    errorCode: Option[String],
                                    errorArgs: Option[Seq[String]],
                                    details: JValue) extends ApiMessage

  private val marshalToHtmlEntity: ToEntityMarshaller[ApiFailureResponse] = defaultNodeSeqMarshaller.compose(_.htmlValue)
  private val marshalToJsonEntity: ToEntityMarshaller[ApiFailureResponse] = jsonTypeMarshaller[ApiFailureResponseJson].compose(_.jsonValue)
  private val marshalToEntity = Marshaller.oneOf(marshalToJsonEntity, marshalToHtmlEntity)

  private val marshalToJsonEntityWithJsonDetails: ToEntityMarshaller[ApiFailureResponse] = jsonTypeMarshaller[ApiFailureResponseJson].compose(_.jsonValueWithJsonDetails)
  private val marshalToEntityWithJsonDetails = Marshaller.oneOf(marshalToJsonEntityWithJsonDetails, marshalToHtmlEntity)

  implicit val apiFailureResponseMarshaller: ToResponseMarshaller[ApiFailureResponse] =
    PredefinedToResponseMarshallers.fromStatusCodeAndValue[StatusCode, ApiFailureResponse](status => status, marshalToEntity).compose(res => (res.statusCode, res))

  def apply(e: ApiFailure): ApiFailureResponse = {
    new ApiFailureResponse(e.status, e.details, Option(e.getMessage), e.errorCode, e.errorArgs)
  }

  implicit val apiFailureResponseJsonUnmarshaller: FromEntityUnmarshaller[ApiFailureResponseJson] = explicitJsonTypeUnmarshaller

}