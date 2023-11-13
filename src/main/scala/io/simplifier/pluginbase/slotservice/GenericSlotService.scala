package io.simplifier.pluginbase.slotservice

import akka.http.scaladsl.model.HttpEntity.{ChunkStreamPart, Chunked}
import akka.http.scaladsl.model.{ContentTypes, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.scaladsl.Source
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginbase.slotservice.Constants._
import io.simplifier.pluginbase.slotservice.GenericFailureHandling.{OperationFailure, OperationFailureMessage}
import io.simplifier.pluginbase.slotservice.GenericRestMessages.RestMessage
import io.simplifier.pluginbase.util.api.{ApiMessage, StreamApiMessage}
import io.simplifier.pluginbase.util.http.JsonMarshalling._
import io.simplifier.pluginbase.util.json.SimplifierFormats
import io.simplifier.pluginbase.util.logging.Logging
import org.json4s.{JValue, MappingException}

import java.sql.SQLException
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

abstract class GenericSlotService extends Logging with SimplifierFormats {

  def slotNames: Seq[String]

  def serviceRoute(implicit requestSource: RequestSource, userSession: UserSession): Route

  def slotError(action: String, aspect: String, error: Throwable): String =
    s"Error during operation $action of $aspect: $error"

  protected def requestHandler[T <: ApiMessage](function: T => Try[ApiMessage],
                                                error: String => RestMessage,
                                                action: String, aspect: String)(implicit manifest: Manifest[T]): Route = {
    entity(as[T])(request =>
      complete(resultHandler(function(request), error, action, aspect))
    ) ~
      entity(as[JValue]) { request =>
        Try {
          request.extract[T]
        } match {
          case Failure(ex) => complete(OperationFailure(error(ex.getMessage), ERROR_CODE_MAPPING_EXCEPTION).toResponse)
          case Success(extractedRequest) => complete(resultHandler(function(extractedRequest), error, action, aspect))
        }
      }
  }

  protected def streamRequestHandler[T <: ApiMessage](function: T => Future[StreamApiMessage],
                                                      error: String => RestMessage,
                                                      action: String, aspect: String
                                                     )(implicit manifest: Manifest[T]): Route = {

    val chunkSource: StreamApiMessage => Source[ChunkStreamPart, _] = entity =>
      Source.fromIterator(() => entity.data).map(ChunkStreamPart.apply)

    val httpResponse: StreamApiMessage => HttpResponse = entity =>
      HttpResponse(entity = Chunked(ContentTypes.`application/octet-stream`, chunkSource(entity)))

    def getResponse(request: T): Route = {
      onComplete(function(request)) { data =>
        complete(resultHandler(data, error, action, aspect) match {
          case e: StreamApiMessage =>
            httpResponse(e)
          case e: ApiMessage => e
        })
      }
    }

    entity(as[T])(request =>
      getResponse(request)
    ) ~
      entity(as[JValue]) { request =>
        Try {
          request.extract[T]
        } match {
          case Failure(ex) => complete(
            OperationFailure(error(ex.getMessage), ERROR_CODE_MAPPING_EXCEPTION).toResponse
          )
          case Success(extractedRequest) =>
            getResponse(extractedRequest)
        }
      }
  }

  def asyncRequestHandler[T <: ApiMessage](function: T => Future[ApiMessage],
                                           error: String => RestMessage,
                                           action: String, aspect: String)(implicit manifest: Manifest[T]): Route ={
    entity(as[T]) { request =>
      onComplete(function(request)) { data =>
        complete(resultHandler(data, error, action, aspect))
      }
    } ~
      entity(as[JValue]) { request =>
        Try {
          request.extract[T]
        } match {
          case Failure(ex) => complete(OperationFailure(error(ex.getMessage), ERROR_CODE_MAPPING_EXCEPTION).toResponse)
          case Success(extractedRequest) =>
            onComplete(function(extractedRequest)) { data =>
              complete(resultHandler(data, error, action, aspect))
            }
        }
      }
  }

  protected def resultHandler(result: Try[ApiMessage], error: String => RestMessage, action: String, aspect: String): ApiMessage = {
    result match {
      case Success(a) => a
      case Failure(failure: OperationFailure) =>
        logger.error(slotError(action, aspect, failure), failure)
        failure.toResponse
      case Failure(fail@OperationFailureMessage(msg, errorCode, statusCode)) =>
        val failure = OperationFailure(error(msg), errorCode, statusCode)
        logger.error(slotError(action, aspect, failure), fail)
        failure.toResponse
      case Failure(ex: MappingException) =>
        logger.error(slotError(action, aspect, ex), ex)
        OperationFailure(error("Unable to parse argument: " + ex.getMessage), ERROR_CODE_MAPPING_EXCEPTION, statusCode = StatusCodes.BadRequest).toResponse
      case Failure(ex: RuntimeException) if ex.getCause.isInstanceOf[SQLException] =>
        logger.error(slotError(action, aspect, ex.getCause), ex)
        OperationFailure(error(ex.getCause.getMessage), ERROR_CODE_RUNTIME_EXCEPTION).toResponse
      case Failure(other) =>
        logger.error(slotError(action, aspect, other), other)
        OperationFailure(error("unexpected error"), ERROR_CODE_UNEXPECTED_EXCEPTION).toResponse
    }
  }

}