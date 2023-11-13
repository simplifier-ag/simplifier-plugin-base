package io.simplifier.pluginbase.definitions

object Constants {


  final protected[pluginbase] val STARTING_PLUGIN: String = "Starting"


  /* Basic State */
  final protected[pluginbase] val LOADING_PLUGIN_DESCRIPTION: String = "Loading the plugin description"
  final protected[pluginbase] val LOADING_PLUGIN_DESCRIPTION_DONE: String = "Loaded the plugin description"

  final protected[pluginbase] val LOADING_PLUGIN_SETTINGS: String = "Loading the plugin settings"
  final protected[pluginbase] val LOADING_PLUGIN_SETTINGS_DONE: String = "Loaded the plugin settings"

  final protected[pluginbase] val ASKING_SIMPLIFIER_INSTANCE_LOCATION: String = "Asking the Simplifier Instance for its location"
  final protected[pluginbase] val ASKING_SIMPLIFIER_INSTANCE_LOCATION_DONE: String = "Simplifier Instance location information retrieved"


  /* Process Step 1 */
  final protected[pluginbase] val INITIALIZING_PLUGIN: String = "Initializing"
  final protected[pluginbase] val INITIALIZING_PLUGIN_DONE: String = "Initialization of"
  final protected[pluginbase] val INITIALIZING_PLUGIN_ERROR: String = "initialization"

  final protected[pluginbase] val DEINITIALIZING_PLUGIN: String = "Deinitializing"
  final protected[pluginbase] val DEINITIALIZING_PLUGIN_DONE: String = "Deinitialization of"

  /* Process Step 2 */
  final protected[pluginbase] val STARTING_SERVICES: String = "Starting services of"
  final protected[pluginbase] val STARTING_SERVICES_DONE: String = "Started services of"
  final protected[pluginbase] val STARTING_SERVICES_ERROR: String = "starting of the services"

  final protected[pluginbase] val STOPPING_SERVICES: String = "Stopping services of"
  final protected[pluginbase] val STOPPING_SERVICES_DONE: String = "Stopped services of"
  final protected[pluginbase] val STOPPING_SERVICES_ERROR: String = "stopping of the services"


  /* Process Step 3 */
  final protected[pluginbase] val STARTING_WEBSERVICE: String = "Starting webservice of"
  final protected[pluginbase] val STARTING_WEBSERVICE_DONE: String = "Started webservice of"
  final protected[pluginbase] val STARTING_WEBSERVICE_ERROR: String = "starting of the webservice"

  final protected[pluginbase] val STOPPING_WEBSERVICE: String = "Stopping webservice of"
  final protected[pluginbase] val STOPPING_WEBSERVICE_DONE: String = "Stopped webservice of"
  final protected[pluginbase] val STOPPING_WEBSERVICE_ERROR: String = "stopping of the webservice"

  /* Process Step 4 */
  final protected[pluginbase] val STARTING_REGISTRATION: String = "Starting registration of"
  final protected[pluginbase] val STARTING_REGISTRATION_DONE: String = "Registered"
  final protected[pluginbase] val STARTING_REGISTRATION_ERROR: String = "registration"

  final protected[pluginbase] val STOPPING_REGISTRATION: String = "Stopping registration of"
  final protected[pluginbase] val STOPPING_REGISTRATION_DONE: String = "Unregistered"
  final protected[pluginbase] val STOPPING_REGISTRATION_ERROR: String = "unregistration"

  /*Process Step 5 */
  final protected[pluginbase] val WAITING_TO_BE_USABLE: String = "Waiting to be usable"

  /* Termination */
  final protected[pluginbase] val TERMINATING_PLUGIN: String = "Terminating"
  final protected[pluginbase] val TERMINATING_PLUGIN_DONE: String = "Terminated"
  final protected[pluginbase] val TERMINATING_PLUGIN_ERROR: String = "termination"

  final protected[pluginbase] val SHUTDOWN: String = "Shutdown"
  final protected[pluginbase] val REMOVING_SHUTDOWN_HOOK: String = "Trying to remove shutdown hook from"
  final protected[pluginbase] val REMOVING_SHUTDOWN_HOOK_DONE: String = "Removed shutdown hook from"
  final protected[pluginbase] val REMOVING_SHUTDOWN_HOOK_ERROR: String = "removal of the shutdown hook"


}
