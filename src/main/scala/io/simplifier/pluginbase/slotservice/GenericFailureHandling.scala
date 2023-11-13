package io.simplifier.pluginbase.slotservice

import akka.http.scaladsl.marshalling.{PredefinedToResponseMarshallers, ToEntityMarshaller, ToResponseMarshaller}
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import io.simplifier.pluginbase.slotservice.GenericRestMessages.RestMessage
import io.simplifier.pluginbase.util.api.ApiMessage
import io.simplifier.pluginbase.util.json.SimplifierFormats

object GenericFailureHandling extends SimplifierFormats {

  case class OperationFailureMessage(msg: String, errorCode: String, statusCode: StatusCode = StatusCodes.InternalServerError) extends Exception(msg)

  case class OperationFailure(msg: RestMessage, errorCode: String, statusCode: StatusCode = StatusCodes.InternalServerError)
    extends Exception(msg.msgText) {

    def toResponse: OperationFailureResponse = OperationFailureResponse(msg, errorCode)

  }

  object OperationFailure {

    implicit def responseMarshaller: ToResponseMarshaller[OperationFailure] =
      PredefinedToResponseMarshallers
        .fromStatusCodeAndValue[StatusCode, OperationFailureResponse](status => status, implicitly[ToEntityMarshaller[OperationFailureResponse]])
        .compose(of => (of.statusCode, of.toResponse))
  }

  case class OperationFailureResponse(message: RestMessage, errorCode: String, success: Boolean = false) extends ApiMessage

}
