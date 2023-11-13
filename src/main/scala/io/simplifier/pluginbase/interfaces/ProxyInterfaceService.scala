package io.simplifier.pluginbase.interfaces

import akka.http.scaladsl.model.StatusCodes.Forbidden
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource

/**
  * Prototype for an HTTP service providing plugin proxy interface
  */
abstract class ProxyInterfaceService {

  /** Base-URL relative to http service root */
  val baseUrl: String

  /** If true, only requests by authenticated users are permitted */
  val authenticationRequired: Boolean = true

  /**
    * Plugin-specific inner route handling proxy requests
    *
    * @param requestSource plugin request source
    * @param userSession   user session
    * @return service route
    */
  def serviceRoute(implicit requestSource: RequestSource, userSession: UserSession): Route


  /**
    * The base route of this service
    *
    * @param requestSource the implicit request source.
    * @param userSession   the implicit user session.
    * @return              the base route.
    */
  def route(implicit requestSource: RequestSource, userSession: UserSession): Route = {
    if (authenticationRequired && userSession.userIdOpt.isEmpty && userSession.tokenOpt.isEmpty) {
      complete(Forbidden -> "Only authenticated access permitted")
    } else {
      serviceRoute
    }
  }

}
