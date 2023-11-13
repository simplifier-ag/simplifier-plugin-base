package io.simplifier.pluginbase.interfaces

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.marshallers.xml.ScalaXmlSupport._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.stream.Materializer
import akka.util.Timeout
import io.simplifier.pluginbase.util.api.ApiExceptionHandler.handleApiFailures
import io.simplifier.pluginbase.util.api.ApiRequestLogger.traceLogHttpTraffic
import io.simplifier.pluginbase.SimplifierPlugin.AppServerInformation
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginDirectives._
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginbase.interfaces.PluginBaseHttpService.DynamicPort
import io.simplifier.pluginbase.security.TrustedSourcesValidator
import io.simplifier.pluginbase.util.logging.Logging
import io.simplifier.pluginbase.{PluginDescription, PluginSettings, StatefulProcess}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.Success

/**
  * Base Http service for Plugin.
  *
  * @param pluginDescription      plugin description object
  * @param pluginSettings         settings
  * @param appServerInformation   app server information
  * @param proxyInterface         attached proxy interface
  * @param slotInterface          attached slot service
  * @param configurationInterface attached configuration service
  * @param system                 actor system
  * @param materializer           actor materializer
  */
class PluginBaseHttpService(pluginDescription: PluginDescription,
                            pluginSettings: PluginSettings,
                            appServerInformation: AppServerInformation,
                            val proxyInterface: Option[ProxyInterfaceService] = None,
                            val slotInterface: Option[SlotInterfaceService] = None,
                            val configurationInterface: Option[ConfigurationInterfaceService] = None,
                            val documentationInterface: Option[DocumentationInterfaceService] = None
                           )
                           (implicit system: ActorSystem, materializer: Materializer) extends Logging {

  import system.dispatcher

  /** service state */
  private val service = StatefulProcess(startHttpService _, stopHttpService)

  protected def startHttpService(): Future[ServerBinding] = {
    implicit val timeout: Timeout = Timeout(5.seconds)
    val bindFuture = Http().newServerAt(pluginSettings.listenInterface, pluginSettings.listenPort).bind(route)
    bindFuture onComplete {
      case Success(binding) => logger.info(s"Plugin WebService listening on ${binding.localAddress}")
      case _ =>
    }
    bindFuture
  }

  protected def stopHttpService(binding: ServerBinding): Future[Unit] = {
    val unbindFuture = binding.unbind().map(_ => ())
    unbindFuture onComplete {
      _ => logger.info("Plugin WebService terminated")
    }
    unbindFuture
  }

  /**
    * Start http base service.
    */
  def start(): Future[DynamicPort] = {
    service.startUp().map {
      binding => DynamicPort(binding.localAddress.getPort)
    }
  }

  /**
    * Stop http base service
    */
  def stop(): Future[Unit] = {
    service.shutDown()
  }

  private def configurationRoute(configInterface: ConfigurationInterfaceService)
                                (implicit requestSource: RequestSource, userSession: UserSession): Route = {
    pathPrefix(configInterface.baseUrl) {
      configInterface.route
    }
  }

  private def proxyRoute(proxyInterface: ProxyInterfaceService)
                        (implicit requestSource: RequestSource, userSession: UserSession): Route = {
    pathPrefix(proxyInterface.baseUrl) {
      proxyInterface.route
    }
  }

  private def slotRoute(slotInterface: SlotInterfaceService)
                       (implicit requestSource: RequestSource, userSession: UserSession): Route = {
    pathPrefix(slotInterface.baseUrl) {
      slotInterface.route
    }
  }

  private val splashScreen =
    <html>
      <title>
        {s"Plugin ${pluginDescription.name}"}
      </title>
      <h1>
        {s"Plugin ${pluginDescription.name}"}
      </h1>
    </html>

  private val splashScreenRoute: Route =
    pathEndOrSingleSlash {
      complete {
        splashScreen
      }
    }

  val route: Route = {
    traceLogHttpTraffic {
      handleApiFailures {
        documentationInterface.map(_.routes).getOrElse(reject) ~
        extractRequestSource {  implicit requestSource =>
          new TrustedSourcesValidator(pluginSettings, appServerInformation).verifyOriginalRequestSource(requestSource) {
            extractUserSession { implicit userSession =>
              splashScreenRoute ~
              proxyInterface.map(proxyRoute).getOrElse(reject) ~
              slotInterface.map(slotRoute).getOrElse(reject) ~
              configurationInterface.map(configurationRoute).getOrElse(reject)
            }
          }
        }
      }
    }
  }

}

object PluginBaseHttpService {

  /**
    * Dynamic port returned during http bind (if no fixed port was defined)
    *
    * @param port port number
    */
  case class DynamicPort(port: Int) extends AnyVal

}