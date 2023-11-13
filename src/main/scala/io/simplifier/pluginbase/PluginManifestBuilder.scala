package io.simplifier.pluginbase

import io.simplifier.pluginapi.PermissionObjectDefinition
import io.simplifier.pluginapi.RegistrationData._
import io.simplifier.pluginbase.interfaces.PluginBaseHttpService
import io.simplifier.pluginbase.interfaces.PluginBaseHttpService.DynamicPort
import io.simplifier.pluginbase.permission.PluginPermissionObject
import io.simplifier.pluginbase.permission.PluginPermissionObject.toApi

/**
  * Plugin manifest construction
  * @param pluginDescription plugin description from plugin.json
  * @param settings plugin settings
  * @param baseService plugin base http service
  * @param dynamicPort dynamic port from web service binding
  * @param permissionObjects permission objects
  */
class PluginManifestBuilder(pluginDescription: PluginDescription,
                            settings: PluginSettings,
                            baseService: PluginBaseHttpService,
                            dynamicPort: DynamicPort,
                            permissionObjects: Seq[PluginPermissionObject]) {

  val details: PluginDetails = PluginDetails(
    name = pluginDescription.name,
    description = pluginDescription.description,
    vendor = pluginDescription.vendor,
    version = pluginDescription.version,
    documentationUrl = pluginDescription.documentationUrl
  )

  val connection: PluginHttpConnection = PluginHttpConnection(
    host = settings.exposedHost,
    port = settings.exposedPort.getOrElse(dynamicPort.port)
  )

  val proxyInterface: Option[PluginProxyInterface] = baseService.proxyInterface map { proxy =>
    PluginProxyInterface(
      baseUrl = proxy.baseUrl,
      authenticationRequired = proxy.authenticationRequired
    )
  }

  val slotInterface: Option[PluginSlotInterface] = baseService.slotInterface map { slot =>
    PluginSlotInterface(
      slot.baseUrl,
      slot.slotNames,
      slot.standaloneOnlySlots
    )
  }

  val configurationInterface: Option[PluginConfigurationInterface] = baseService.configurationInterface map { cfg =>
    PluginConfigurationInterface(
      baseUrl = cfg.baseUrl,
      views = cfg.views
    )
  }

  val permissions: Option[Seq[PermissionObjectDefinition]] = Some(permissionObjects map toApi).filter(_.nonEmpty)

  val manifest: PluginManifest = PluginManifest(details, connection, proxyInterface, slotInterface, configurationInterface, permissions)

}

object PluginManifestBuilder {

  /**
    * Build plugin manifest.
    * @param pluginDescription plugin description from plugin.json
    * @param settings plugin settings
    * @param baseService plugin base http service
    * @param dynamicPort dynamic port from web service binding
    * @param permissionObjects permission objects
    * @return plugin manifest
    */
  def apply(pluginDescription: PluginDescription,
            settings: PluginSettings,
            baseService: PluginBaseHttpService,
            dynamicPort: DynamicPort,
            permissionObjects: Seq[PluginPermissionObject]): PluginManifest =
    new PluginManifestBuilder(pluginDescription, settings, baseService, dynamicPort, permissionObjects).manifest

}