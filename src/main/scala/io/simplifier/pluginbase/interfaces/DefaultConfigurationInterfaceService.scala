package io.simplifier.pluginbase.interfaces

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.simplifier.pluginapi.RegistrationData.PluginConfigurationView
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource

/**
  * Default configuration interface, service configuration files from a folder in classpath.
  *
  * @param baseUrl            base url of the service
  * @param resourceDirectory  resource directory (relative to "resources" folder)
  * @param views              declared views for admin UI
  */
class DefaultConfigurationInterfaceService(val baseUrl: String, resourceDirectory: String, val views: Seq[PluginConfigurationView])
  extends ConfigurationInterfaceService {

  /** Service route to deliver assets */
  override def serviceRoute(implicit requestSource: RequestSource, userSession: UserSession): Route = {
    withPrecompressedMediaTypeSupport {
      getFromResourceDirectory(resourceDirectory)
    }
  }
}
