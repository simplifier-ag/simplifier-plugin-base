package io.simplifier.pluginbase.helpers

import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginapi.serverSettings.ReadServerSettings.Settings
import io.simplifier.pluginbase.interfaces.AppServerDispatcher
import io.simplifier.pluginbase.util.logging.DeferredLogging

import scala.concurrent.ExecutionContext

/**
  * containing the state of the settings of the simplifier server;
  *
  * the state will be updated, if new settings are saved in simplifier.
  */
class SimplifierServerSettings(appServerDispatcher: AppServerDispatcher) extends DeferredLogging {

  var serverSettings: Option[Settings] = None

  private def init(implicit ec: ExecutionContext, userSession: UserSession, requestSource: RequestSource): SimplifierServerSettings = {
    fetchSettingsFromServer
    this
  }

  private def refresh(implicit ec: ExecutionContext, userSession: UserSession, requestSource: RequestSource): String = {
    logger.info("refresh of server settings triggered")
    fetchSettingsFromServer
    "refresh server settings triggered"
  }

  private def fetchSettingsFromServer(implicit ec: ExecutionContext, userSession: UserSession, requestSource: RequestSource): Unit = {
    logger.trace("next calling getEnablePluginPermissionCheck")
    appServerDispatcher.getEnablePluginPermissionCheck()
      .recover {
        case e: Throwable =>
          logger.error(s"Error calling getEnablePluginPermissionCheck", e)
          Settings(activatePluginPermissionCheck = true)
      }.map(value => {
      logger.debug(s"Received result $value from simplifier server")
      serverSettings = Some(value)
    })
    logger.trace("next calling ...DONE ")
  }
}


object SimplifierServerSettings {

  var globalSimplifierServerSettings: Option[SimplifierServerSettings] = None

  /**
    * called at plugin startup
    */
  def initServerSettings(appServerDispatcher: AppServerDispatcher)(implicit ec: ExecutionContext,
                                                                   userSession: UserSession, requestSource: RequestSource): Unit = {
    globalSimplifierServerSettings = Some(new SimplifierServerSettings(appServerDispatcher).init)
  }

  /**
    * called every time a new setting is saved in the simplifier server
    */
  def reFresh(implicit ec: ExecutionContext, userSession: UserSession, requestSource: RequestSource): String = {
    globalSimplifierServerSettings.getOrElse(throw new RuntimeException("probably globalSimplifierServerSettings not yet initialized"))
      .refresh
  }

  /**
    * to be used inside the plugins to decide, whether an enhanced permission check should be done
    *
    * NOTE: if some plugins do a check of the settings at the very beginning after startup, this might not yet be
    * initialized - in that case we can think of implementing it as a Future
    *
    */
  def activatePluginPermissionCheck: Boolean =
    globalSimplifierServerSettings.getOrElse(throw new RuntimeException("probably globalSimplifierServerSettings not yet initialized"))
      .serverSettings.getOrElse(throw new RuntimeException("serverSettings of globalSimplifierServerSettings not yet fetched from server"))
      .activatePluginPermissionCheck

}
