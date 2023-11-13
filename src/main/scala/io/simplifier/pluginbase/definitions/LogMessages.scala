package io.simplifier.pluginbase.definitions

import io.simplifier.pluginbase.PluginDescription

object LogMessages {


  def Action(action: String): String = s"$action."

  def ActionDone(action: String): String = s"$action successfully."


  def PluginAction(action: String, pluginDescription: PluginDescription): String =
    s"$action plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}}."

  def PluginActionDone(action: String, pluginDescription: PluginDescription): String =
    s"$action plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}} successfully."

  def PluginActionAlreadyInProgress(action: String, pluginDescription: PluginDescription): String =
    s"$action of plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}} is already in progress."


  def PluginUsable(pluginDescription: PluginDescription): String =
    s"Plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}} started successfully and is ready to be used."


  def StartingWithArguments(args: Array[String]): String =
    s"Starting this plugin with the following command line arguments: {${Option(args).map(_.mkString(", ")).getOrElse("")}}"


  def ErrorDuringPluginAction(action: String, error: Throwable, pluginDescription: PluginDescription): String =
    s"The error: {${error.getClass.getName}} occurred during the $action of plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}}."

  def TerminationCause(error: Throwable, pluginDescription: PluginDescription): String =
    s"The error, that caused the applying of the termination process of plugin: {${pluginDescription.name}} " +
    s"in version: {${pluginDescription.version}} was the following one: {${error.getClass.getName}} its message is: {${error.getMessage}}."

  def TerminatingPluginBySystemShutdown(pluginDescription: PluginDescription): String =
    s"Terminating Plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}} by system shutdown."

  def TerminatedPluginBySystemShutdown(pluginDescription: PluginDescription): String =
    s"Terminated plugin by system shutdown: {${pluginDescription.name}} in version: {${pluginDescription.version}} successfully."

  def NoBaseServicesProvided(pluginDescription: PluginDescription): String =
    s"This plugin ({${pluginDescription.name}} in version: {${pluginDescription.version}}) " +
    s"does not provide any interfaces in the base service. Please override startPluginServices!"

  def ConfigLoaded(location: String): String = s"Loading configuration from the location: {$location}."

  def ConfigLoadedDone(location: String): String = s"Configuration from the location: {$location} loaded successfully."


}
