package io.simplifier.pluginbase.interfaces

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.simplifier.pluginapi.RegistrationData.PluginConfigurationView
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource

/**
  * Prototype for an HTTP service providing plugin configuration/assets interface
  */
abstract class ConfigurationInterfaceService {

  /** Base-URL relative to http service root */
  val baseUrl: String

  /** Declared views for admin UI */
  def views: Seq[PluginConfigurationView]

  /** Service route to deliver assets */
  def serviceRoute(implicit requestSource: RequestSource, userSession: UserSession): Route

  /**
    * The base route of this service
    *
    * @param requestSource the implicit request source.
    * @param userSession   the implicit user session.
    * @return              the base route.
    */
  def route(implicit requestSource: RequestSource, userSession: UserSession): Route = {
    get {
      serviceRoute
    }
  }
}
