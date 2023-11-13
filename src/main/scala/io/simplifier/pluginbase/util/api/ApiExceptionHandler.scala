package io.simplifier.pluginbase.util.api

import akka.http.scaladsl.server.{Directive0, Directives, ExceptionHandler}
import io.simplifier.pluginbase.util.logging.Logging

/**
  * Exception Handler for API routes.
 *
  * @author Christian Simon
  */
object ApiExceptionHandler extends Logging with Directives {

  // Logging Type
  type ExtendedLoggerFunc = Throwable => Unit

  @volatile
  var extendedLogger: Option[ExtendedLoggerFunc] = None

  private val handle: ExceptionHandler.PF = {
    case anyError: Throwable =>
      try {
        anyError match {
          case ApiFailure(msg, errorToReport, response) =>
            errorToReport.foreach { e =>
              logger.error(msg, e)
              extendedLogger.foreach(_(e)) // Apply extended logging if enabled
            }
            complete(response)
        }
      } catch {
        case e: Throwable =>
          e.printStackTrace()
          complete(e.getMessage)
      }
  }

  private val exceptionHandler = ExceptionHandler(handle)

  /**
    * Directive to handle API failures.
    */
  val handleApiFailures: Directive0 = handleExceptions(exceptionHandler)

}
