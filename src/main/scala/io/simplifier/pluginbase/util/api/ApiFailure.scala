package io.simplifier.pluginbase.util.api

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.model.StatusCode
import akka.http.scaladsl.model.StatusCodes.InternalServerError
import PredefinedApiFailures.UnexpectedErrorFailure
import WrappedExceptions.{TranslatableWrappedException, WrappedException}
import org.json4s._
import org.json4s.JValue

import scala.util.control.NonFatal

/**
  * An API failure indicates a specific exceptional situation in and API or Controller execution,
  * preventing the function to finish in a regular way. This can be a validation check, technical problem or
  * logical exception. An API failure is tightly bound to the status and message relevant for the user, and
  * is generally designed to be returned to the user via a HTTP response.
  *
  * @see ApiFailureResponse
  *
  * @param message message to display. If no message is given, the default message of the status code is taken
  * @param cause throwable cause; if given together with reportError=true, the stacktrace of the cause will be printed
  * @param details optional JSON-Details, which will be sent.
  * @param status status code of the generated http response (500 internal server error, by default)
  * @param reportError flag, if the cause should be printed as stacktrace. Use this for failures, which describe a
  *                    problem in the server, not a wrong API use of the user
  * @param errorCode fixed error code to render in the response (optional)
  * @param errorArgs argument to the error code (optional), in order to generate a full i18n message
  */
class ApiFailure(message: String,
                 cause: Throwable = null,
                 val details: JValue = JNothing,
                 val status: StatusCode = InternalServerError,
                 val reportError: Boolean = false,
                 val errorCode: Option[String] = None,
                 val errorArgs: Option[Seq[String]] = None
                ) extends Exception(Option(message).getOrElse(""), cause) {
  /**
    * Return corresponding [[ApiFailureResponse]] for this failure.
    * @return api failure response
    */
  def asResponse: ApiFailureResponse = ApiFailureResponse(this)

}

object ApiFailure {

  /**
    * Create failure from message, optional error to report and response.
    * @param message message to display
    * @param errorToReport error to report (optional)
    * @param response failure response
    * @return api failure
    */
  def apply(message: String, errorToReport: Option[Throwable], response: ApiFailureResponse): ApiFailure =
    new ApiFailure(message, errorToReport.orNull, JNothing, response.statusCode, errorToReport.isDefined,
      response.errorCode, response.errorArgs)

  /**
    * Create "internal server error" failure from underlying throwable, or if throwable is already an api failure,
    * return the original failure.
    * @param e throwable as cause
    * @return api failure
    */
  def apply(e: Throwable): ApiFailure = e match {
    case apiFailure: ApiFailure => apiFailure
    case NonFatal(cause: WrappedException) => cause.asApiFailure
    case NonFatal(cause: TranslatableWrappedException) => cause.asApiFailure
    case cause => new UnexpectedErrorFailure(null, cause = cause)
  }

  /**
    * Extract api failure response from throwable.
    * @param e throwable to match
    * @return Tuple: (message, throwable to report, response object)
    */
  def unapply(e: Throwable): Option[(String, Option[Throwable], ApiFailureResponse)] = {
    val apiFailure = e match {
      case apiFailure: ApiFailure => Some(apiFailure)
      case NonFatal(cause) => Some(ApiFailure(cause))
      case _ => None
    }
    apiFailure map { af => (
      af.getMessage,
      if(af.reportError) Option(af.getCause) else None,
      ApiFailureResponse(af)
    )}
  }

  implicit val apiFailureMarshaller: ToResponseMarshaller[ApiFailure] =
    ApiFailureResponse.apiFailureResponseMarshaller.compose(ApiFailureResponse.apply)
}