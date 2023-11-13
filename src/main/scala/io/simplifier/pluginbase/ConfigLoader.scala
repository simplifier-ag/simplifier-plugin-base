package io.simplifier.pluginbase

import io.simplifier.pluginbase.util.logging.Logging
import com.typesafe.config.{Config, ConfigFactory}

import java.io.File
import java.net.URL

/**
  * Loader for LightBend config.
  */
class ConfigLoader(args: Array[String], defaultLocation: String) {

  import ConfigLoader._

  /**
    * Get the commandline parameter describing the location of th the settings
    */
  def getConfigArgument: Option[String] = Option(args).flatMap(_.headOption)

  private def configArgumentAsConf: Option[String] = getConfigArgument map { name =>
    if (name endsWith ".conf") name else s"$name.conf"
  }

  def getConfigSource: SettingsSource = configArgumentAsConf match {
    case None => DefaultSettings(defaultLocation)
    case Some(ExistingFile(file)) => SettingsFromFile(file)
    case Some(resourceName@ExistingClassPathResource(_)) => SettingsFromResource(resourceName)
    case Some(location) => SettingsNotFound(location)
  }

  def loadConfig(): Config = getConfigSource.load()


}

object ConfigLoader extends Logging {

  /**
    * Create config loader.
    *
    * @param args            commandline arguments
    * @param defaultLocation default location of settings
    * @return                settings loader
    */
  def apply(args: Array[String], defaultLocation: String = "settings"): ConfigLoader =
    new ConfigLoader(args, defaultLocation)

  sealed trait SettingsSource {
    def load(): Config
  }

  case class DefaultSettings(resourceName: String) extends SettingsSource {
    def load(): Config = {
      logger.debug("Loading default settings.")
      val config: Config = ConfigFactory.load(resourceName)
      logger.debug("Default settings loaded successfully.")
      config
    }
  }

  case class SettingsFromFile(file: File) extends SettingsSource {
    def load(): Config = {
      logger.debug(s"Loading settings from file: {$file}.")
      val config: Config = ConfigFactory.parseFileAnySyntax(file)
      logger.debug(s"Loaded settings from file: {$file} successfully.")
      config
    }
  }

  case class SettingsFromResource(resourceName: String) extends SettingsSource {
    def load(): Config = {
      logger.debug(s"Loading settings from classpath: {$resourceName}.")
      val config: Config = ConfigFactory.load(resourceName)
      logger.debug(s"Loaded settings from classpath: {$resourceName} successfully.")
      config
    }
  }

  case class SettingsNotFound(location: String) extends SettingsSource {
    def load() = throw new IllegalArgumentException(s"Settings not found at: {$location}")
  }

  /**
   * Extractor for an existing file from a path name.
   */
  object ExistingFile {
    def unapply(path: String): Option[File] = {
      Some(new File(path)).filter(_.isFile)
    }
  }

  /**
   * Extractor for existing classpath resource from an url.
   */
  object ExistingClassPathResource {
    def unapply(url: String): Option[URL] = {
      Option(getClass.getResource(url))
    }
  }

}