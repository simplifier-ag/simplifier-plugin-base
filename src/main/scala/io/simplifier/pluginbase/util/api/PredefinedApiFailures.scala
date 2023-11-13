package io.simplifier.pluginbase.util.api

import akka.http.scaladsl.model.StatusCodes
import org.json4s.{JValue, _}

import scala.language.implicitConversions

/**
  * Pre-defined API Failures.
  * These general failures can be used to describe  situation in APIs and Controllers, and can also be extended
  * for individual exceptional situations.
  */
object PredefinedApiFailures {

  /**
    * Formalized factory companion object for ApiFailures
    *
    * @tparam T the ApiFailure type to construct
    */
  trait ApiFailureFactory[T <: ApiFailure] {

    def apply(message: String, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing): T

  }

  /**
    * Failure indicating there was a technical error (Exception) which should also be logged.
    * This is the fallback for all unhandled Exceptions during API evaluation.
    * Optional details can be included as well
    *
    * @param message   message to display
    * @param errorCode optional error code
    * @param errorArgs optional arguments for the error code
    * @param details   optional JSON-Details, which will be sent
    * @param cause     throwable cause
    */
  class UnexpectedErrorFailure(message: String = null,
                               errorCode: Option[String] = None,
                               errorArgs: Option[Seq[String]] = None,
                               details: JValue = JNothing,
                               cause: Throwable = null)
    extends ApiFailure(message, cause, details, reportError = true, errorCode = errorCode, errorArgs = errorArgs)

  object UnexpectedErrorFailure {
    def apply() = new UnexpectedErrorFailure()

    def apply(cause: Throwable) = new UnexpectedErrorFailure(cause = cause)

    def apply(message: String) = new UnexpectedErrorFailure(message = message)

    def apply(message: String, details: JValue) = new UnexpectedErrorFailure(message = message, details = details)

    def apply(message: String, cause: Throwable) = new UnexpectedErrorFailure(message, cause = cause)

    def apply(message: String, details: JValue, cause: Throwable) =
      new UnexpectedErrorFailure(message, details = details, cause = cause)

    def apply(message: String, errorCode: Option[String], errorArgs: Option[Seq[String]]) =
      new UnexpectedErrorFailure(message = message, errorCode = errorCode, errorArgs = errorArgs)

    def apply(message: String, cause: Throwable = null, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None): UnexpectedErrorFailure =
      new UnexpectedErrorFailure(message = message, cause = cause, errorCode = errorCode, errorArgs = errorArgs)

    def apply(message: String, details: JValue, cause: Throwable, errorCode: Option[String], errorArgs: Option[Seq[String]]) =
      new UnexpectedErrorFailure(message = message, cause = cause, errorCode = errorCode, errorArgs = errorArgs, details = details)
  }


  /**
    * Failure indicating, that the resource is not supported or not implemented
    *
    * @param message   message to display. If no message is given, the default message of the status code is taken
    * @param details   optional JSON-Details, which will be sent.
    * @param errorCode fixed error code to render in the response (optional)
    * @param errorArgs argument to the error code (optional), in order to generate a full i18n message
    */
  class UnsupportedFeatureFailure(message: String, details: JValue, errorCode: Option[String], errorArgs: Option[Seq[String]])
    extends ApiFailure(message, status = StatusCodes.NotImplemented, errorCode = errorCode, errorArgs = errorArgs, details = details)


  object UnsupportedFeatureFailure extends ApiFailureFactory[UnsupportedFeatureFailure] {
    def apply(message: String = "Accessed resource is not implemented", errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new UnsupportedFeatureFailure(message, details, errorCode, errorArgs)
  }


  /**
   * Failure indicating that the main resource of the call, queried by id or another unique identifier, was not found.
   *
   * @param id        identifier value that was not found
   * @param idName    identifier name that was searched (like "id" or "name")
   * @param errorCode optional error code
   * @param errorArgs optional arguments for the error code
   * @param details   optional JSON-Details, which will be sent
   */
  class IdNotFoundFailure(id: Any, idName: String, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing)
    extends NotFoundFailure(s"$idName '$id' not found", errorCode = errorCode, errorArgs = errorArgs, details = details)

  object IdNotFoundFailure {
    def apply(id: Any, idName: String = "ID", errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new IdNotFoundFailure(id, idName, errorCode, errorArgs, details)
  }


  /**
    * Failure indicating that a request could not be executed, because the process ran into an invalid state,
    * or because an affected entity had an unrecoverable invalid state.
    *
    * @param message   message to display
    * @param errorCode optional error code
    * @param errorArgs optional arguments for the error code
    * @param details   optional JSON-Details, which will be sent
    */
  class InvalidInternalStateFailure(message: String, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing)
    extends ApiFailure(message, status = StatusCodes.InternalServerError, errorCode = errorCode, errorArgs = errorArgs, details = details)

  object InvalidInternalStateFailure extends ApiFailureFactory[InvalidInternalStateFailure] {
    def apply(message: String = "Invalid Internal State", errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new InvalidInternalStateFailure(message, errorCode, errorArgs, details)
  }

  /**
    * Failure indicating a bad user request.
    *
    * @param message   message to display
    * @param errorCode optional error code
    * @param errorArgs optional arguments for the error code
    * @param details   optional JSON-Details, which will be sent
    */
  class BadRequestFailure(message: String, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing)
    extends ApiFailure(message, status = StatusCodes.BadRequest, errorCode = errorCode, errorArgs = errorArgs, details = details)

  object BadRequestFailure extends ApiFailureFactory[BadRequestFailure] {
    def apply(message: String = "Bad Request", errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new BadRequestFailure(message, errorCode, errorArgs, details)
  }

  /**
    * Failure indicating a missing resource
    *
    * @param message   message to display
    * @param errorCode optional error code
    * @param errorArgs optional arguments for the error code
    * @param details   optional JSON-Details, which will be sent
    */
  class NotFoundFailure(message: String, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing)
    extends ApiFailure(message, status = StatusCodes.NotFound, errorCode = errorCode, errorArgs = errorArgs, details = details)

  object NotFoundFailure extends ApiFailureFactory[NotFoundFailure] {
    def apply(message: String = "Resource not found", errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new NotFoundFailure(message, errorCode, errorArgs, details)
  }

  /**
    * Failure indicating that there was a conflict when executing the request (like an already existing element with
    * the same unique identifier when creating a resource).
    *
    * @param message   message to display
    * @param details   optional JSON-Details, which will be sent
    * @param errorCode optional error code
    * @param errorArgs optional arguments for the error code
    */
  class ConflictFailure(message: String, details: JValue = JNothing, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None)
    extends ApiFailure(message, status = StatusCodes.Conflict, details = details, errorCode = errorCode, errorArgs = errorArgs)

  object ConflictFailure extends ApiFailureFactory[ConflictFailure] {
    def apply(message: String = "Conflict happened", errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new ConflictFailure(message, details, errorCode, errorArgs)
  }

  /**
    * Failure indicating the access to a resource or the execution of a function was forbidden
    * due to insufficient permissions.
    *
    * @param message   message to display
    * @param errorCode optional error code
    * @param errorArgs optional arguments for the error code
    */
  class ForbiddenFailure(message: String, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing)
    extends ApiFailure(message, status = StatusCodes.Forbidden, errorCode = errorCode, errorArgs = errorArgs, details = details)

  object ForbiddenFailure extends ApiFailureFactory[ForbiddenFailure] {
    def apply(message: String = "You are not authorized to execute this operation", errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new ForbiddenFailure(message, errorCode, errorArgs, details)
  }

  /**
    * Common failure class for situations where the provided payload / parameter body was not computable or could
    * not be referenced.
    *
    * @param message   Use this parameter to override the default message
    * @param errorCode optional error code
    * @param errorArgs optional arguments for the error code
    */
  class ParametersFailure(message: String, errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing)
    extends ApiFailure(message, status = StatusCodes.UnprocessableEntity, errorCode = errorCode, errorArgs = errorArgs, details = details)

  /**
    * Companion object
    *
    * @see [[PredefinedApiFailures.ParametersFailure]]
    */
  object ParametersFailure extends ApiFailureFactory[ParametersFailure] {
    def apply(message: String = "The provided payload information (e.g. parameters) was wrong or could not be referenced.",
              errorCode: Option[String] = None, errorArgs: Option[Seq[String]] = None, details: JValue = JNothing) =
      new ParametersFailure(message, errorCode, errorArgs, details)
  }

}