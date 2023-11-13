package io.simplifier.pluginbase

import io.simplifier.pluginbase.util.json.SimplifierFormats
import io.simplifier.pluginbase.util.logging.Logging
import org.apache.commons.io.IOUtils
import org.json4s.jackson.Serialization.read

import java.net.URL
import java.nio.charset.StandardCharsets
import scala.util.{Failure, Success, Try}


case class PluginDescription(name: String,
                             description: String,
                             version: String,
                             vendor: String,
                             documentationUrl: Option[String])

object PluginDescription extends SimplifierFormats with Logging{

  private def ResourceNotFoundException(resource:String): IllegalStateException = new IllegalStateException(s"Resource: {$resource} was not found in classpath.")
  final private val RESOURCE: String = "plugin.json"



  /**
    * Loads the plugin description from the plugin.json file.
    *
    * @return the Plugin Description Case Class.
    */
  def load(): PluginDescription = {
    logger.debug(s"Loading plugin definition from resource: {$RESOURCE}.")
    val url:URL = Option(getClass.getClassLoader.getResource(RESOURCE)).getOrElse(throw ResourceNotFoundException(RESOURCE))
    logger.debug(s"Loaded plugin definition from resource: {$RESOURCE} successfully.")

    logger.debug(s"Transforming plugin definition into a string.")
    val jsonString:String = IOUtils.toString(url, StandardCharsets.UTF_8)
    logger.debug(s"Transformed plugin definition into a string successfully.")

    logger.debug(s"Extracting plugin definition from the string.")
    Try(read[PluginDescription](jsonString)) match {
      case Success(res) => logger.trace(s"Plugin description contains the following entries: {Name: ${res.name}, Description: ${res.description}, " +
                                        s"Version: ${res.version}, Vendor: ${res.vendor}, ${res.documentationUrl.getOrElse("Not Available")}")
        logger.debug(s"Extracted plugin definition from the string successfully.")
        res
      case Failure(e) =>
        logger.error(s"The error: {${e.getClass.getName}} occurred during the loading of the plugin description its message was: {${e.getMessage}}.")
        throw e
    }
  }

}