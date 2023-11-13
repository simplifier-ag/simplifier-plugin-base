package io.simplifier.pluginbase.util.api

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.{Directive0, RouteResult}
import akka.http.scaladsl.server.directives.{DebuggingDirectives, LoggingMagnet}
import io.simplifier.pluginbase.util.logging.Logging
import org.joda.time.DateTime

/**
  * Helper directive to enable logging of all requests and responses in an API base route.
  */
object ApiRequestLogger extends Logging {

  val traceLogHttpTraffic: Directive0 = DebuggingDirectives.logRequestResult(LoggingMagnet(_ => logHttpTraffic))

  private def logHttpTraffic(req: HttpRequest): RouteResult => Unit = {
    if (logger.isTraceEnabled) logger.trace("Received request: {}", req)
    val receiveTime = DateTime.now()
    res => {
      val duration = DateTime.now().getMillis - receiveTime.getMillis
      if (logger.isTraceEnabled) logger.trace("Produced response (in {} ms): {}", duration, res)
    }
  }

}
