package io.simplifier.pluginbase

import akka.util.Timeout
import com.typesafe.config.Config
import io.simplifier.pluginbase.PluginDescription.logger
import io.simplifier.pluginbase.definitions.InternalDataHandling.{BlackAndWhiteList, IncomingRequests, Security}
import io.simplifier.pluginbase.util.config.ConfigExtension._

import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._

case class PluginSettings(listenInterface: String,
                          listenPort: Int,
                          exposedHost: String,
                          exposedPort: Option[Int],
                          simplifierHost: String,
                          simplifierPort: Int,
                          pluginSecret: String,
                          timeoutDuration: FiniteDuration,
                          shutdownTimeout: FiniteDuration,
                          maximumTimeout: FiniteDuration,
                          security: Security) {


  val timeout: Timeout = Timeout(timeoutDuration)

}

object PluginSettings {

  def UsingDefaultValue(path: String, value: Any): String = s"The parsed value for path: {$path} could not be retrieved! The default value: {$value} will be used instead!"

  def ValueNotProvided(path: String, `type`: String): String = s"The value of type: {${`type`}} for path: {$path} was not provided!"

  val LISTENER_INTERFACE_PATH: String = "plugin.http.interface"
  val LISTENER_PORT_PATH: String = "plugin.http.port"
  val LISTENER_EXPOSED_HOST_PATH: String = "plugin.http.exposedHost"
  val LISTENER_EXPOSED_PORT_PATH: String = "plugin.http.exposedPort"

  val REGISTRATION_SIMPLIFIER_HOST_PATH: String = "plugin.registration.exposedHost"
  val REGISTRATION_SIMPLIFIER_PORT_PATH: String = "plugin.registration.port"

  val TIMEOUT_DURATION_PATH: String = "plugin.timeoutSeconds"
  val SHUTDOWN_TIMEOUT_DURATION_PATH: String = "plugin.shutdownTimeout"
  val MAXIMUM_TIMEOUT_DURATION_PATH: String = "plugin.maximumTimeout"

  val SECURITY_TRUSTED_SOURCES_BLACKLIST_PATH: String = "security.incomingRequests.trustedSources.blackList"
  val SECURITY_TRUSTED_SOURCES_WHITELIST_PATH: String = "security.incomingRequests.trustedSources.whiteList"
  val SECURITY_ALLOW_UNTRUSTED_SOURCES_PATH: String = "security.incomingRequests.allowUntrustedSources"

  val LISTENER_INTERFACE_DEFAULT: String = "0.0.0.0"
  val LISTENER_PORT_DEFAULT: Int = 0

  val REGISTRATION_SIMPLIFIER_HOST_DEFAULT: String = "127.0.0.1"
  val REGISTRATION_SIMPLIFIER_HOST_PORT: Int = 8085

  val TIMEOUT_DURATION_DEFAULT: FiniteDuration = FiniteDuration(60L, SECONDS)
  val SHUTDOWN_TIMEOUT_DURATION_DEFAULT: FiniteDuration = FiniteDuration(60L, SECONDS)
  val MAXIMUM_TIMEOUT_DURATION_DEFAULT: FiniteDuration = FiniteDuration(180, SECONDS)

  val SECURITY_TRUSTED_SOURCES_BLACKLIST_DEFAULT: Seq[String] = Seq.empty[String]
  val SECURITY_TRUSTED_SOURCES_WHITELIST_DEFAULT: Seq[String] = Seq.empty[String]
  val SECURITY_ALLOW_UNTRUSTED_SOURCES_DEFAULT: Boolean = true

  /**
    * Build PluginSettings from configuration.
    *
    * @param config configuration
    * @return plugin settings
    */
  def apply(config: Config, pluginSecret: String): PluginSettings = {

    logger.debug("Retrieving configuration arguments.")
    val listenInterface: String = config
      .getOpt[String](LISTENER_INTERFACE_PATH)
      .getOrElse(logAndReturn(LISTENER_INTERFACE_PATH, LISTENER_INTERFACE_DEFAULT))
    val listenPort: Int = config
      .getOpt[Int](LISTENER_PORT_PATH)
      .getOrElse(logAndReturn(LISTENER_PORT_PATH, LISTENER_PORT_DEFAULT))
    val exposedHost: String = config
      .getOpt[String](LISTENER_EXPOSED_HOST_PATH)
      .getOrElse(logAndReturn(LISTENER_EXPOSED_HOST_PATH, REGISTRATION_SIMPLIFIER_HOST_DEFAULT))
    val exposedPort: Option[Int] = config
      .getOpt[Int](LISTENER_EXPOSED_PORT_PATH)
    val simplifierHost: String = config
      .getOpt[String](REGISTRATION_SIMPLIFIER_HOST_PATH)
      .getOrElse(logAndReturn(REGISTRATION_SIMPLIFIER_HOST_PATH, REGISTRATION_SIMPLIFIER_HOST_DEFAULT))
    val simplifierPort: Int = config
      .getOpt[Int](REGISTRATION_SIMPLIFIER_PORT_PATH)
      .getOrElse(logAndReturn(REGISTRATION_SIMPLIFIER_PORT_PATH, REGISTRATION_SIMPLIFIER_HOST_PORT))
    val timeoutDuration: FiniteDuration = config
      .getOpt[Int](TIMEOUT_DURATION_PATH).fold(logAndReturn(TIMEOUT_DURATION_PATH, TIMEOUT_DURATION_DEFAULT))(FiniteDuration(_, SECONDS))
    val shutdownTimeoutDuration: FiniteDuration = config
      .getOpt[Int](SHUTDOWN_TIMEOUT_DURATION_PATH).fold(logAndReturn(SHUTDOWN_TIMEOUT_DURATION_PATH, SHUTDOWN_TIMEOUT_DURATION_DEFAULT))(FiniteDuration(_, SECONDS))
    val maximumTimeoutDuration: FiniteDuration = config
      .getOpt[Int](MAXIMUM_TIMEOUT_DURATION_PATH).fold(logAndReturn(MAXIMUM_TIMEOUT_DURATION_PATH, MAXIMUM_TIMEOUT_DURATION_DEFAULT))(FiniteDuration(_, SECONDS))

    val blackList: Seq[String] = config
      .getStringListOpt(SECURITY_TRUSTED_SOURCES_BLACKLIST_PATH).fold(logAndReturn(SECURITY_TRUSTED_SOURCES_BLACKLIST_PATH, SECURITY_TRUSTED_SOURCES_BLACKLIST_DEFAULT))(bl => bl)
    val whiteList: Seq[String] = config
      .getStringListOpt(SECURITY_TRUSTED_SOURCES_WHITELIST_PATH).fold(logAndReturn(SECURITY_TRUSTED_SOURCES_WHITELIST_PATH, SECURITY_TRUSTED_SOURCES_WHITELIST_DEFAULT))(wl => wl)
    val allowUntrustedSources: Boolean = config.
      getOpt[Boolean](SECURITY_ALLOW_UNTRUSTED_SOURCES_PATH).fold(logAndReturn(SECURITY_ALLOW_UNTRUSTED_SOURCES_PATH, SECURITY_ALLOW_UNTRUSTED_SOURCES_DEFAULT))(auts => auts)

    (allowUntrustedSources, blackList, whiteList) match {
      case (false, bl, wl) if bl.isEmpty && wl.isEmpty => throw new WrongConfigurationException("an empty trusted source blacklist and whitelist albeit one of them was expected")
      case (false, bl, wl) if bl.contains(null) && wl.contains(null) => throw new WrongConfigurationException("trusted source blacklist and whitelist contains null values")
      case (false, bl, _) if bl.contains(null)  => throw new WrongConfigurationException("trusted source blacklist contains null values")
      case (false, _, wl) if wl.contains(null) => throw new WrongConfigurationException("trusted source whitelist contains null values")
      case _ => //NOP
    }

    logger.debug("Configuration arguments retrieved successfully.")

    logger.debug("Constructing plugin settings object.")
    val pluginSettings: PluginSettings = PluginSettings(listenInterface, listenPort, exposedHost, exposedPort, simplifierHost, simplifierPort, pluginSecret, timeoutDuration,
      shutdownTimeoutDuration, maximumTimeoutDuration, Security(IncomingRequests(BlackAndWhiteList(blackList, whiteList), allowUntrustedSources = allowUntrustedSources)))
    logger.debug("Plugin settings object was successfully constructed.")
    pluginSettings
  }


  private def logAndReturn[T](path: String, value: T): T = {
    logger.warn(UsingDefaultValue(path, value))
    value
  }


  private def paramFailure[T: TypeTag : ClassTag](msg: String): T = {
    logger.error(ValueNotProvided(msg, typeOf[T].toString))
    throw new PluginSettingsMissingException(msg)
  }

  class PluginSettingsMissingException(path: String) extends Exception(s"Plugin configuration is missing the configuration setting: {$path}")
  class WrongConfigurationException(reason: String) extends Exception(s"The provided configuration is invalid due to $reason")

}