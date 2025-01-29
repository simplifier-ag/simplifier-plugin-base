package io.simplifier.pluginbase

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.Config
import io.simplifier.pluginapi.RegistrationData.PluginManifest
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.{Plugin, RequestSource}
import io.simplifier.pluginbase.SimplifierPlugin._
import io.simplifier.pluginbase.definitions.Constants._
import io.simplifier.pluginbase.definitions.LogMessages._
import io.simplifier.pluginbase.helpers.{FileWatch, RoleCacheActor, RoleCaches, SimplifierServerSettings}
import io.simplifier.pluginbase.interfaces.AppServerDispatcher.Routes
import io.simplifier.pluginbase.interfaces._
import io.simplifier.pluginbase.permission.PluginPermissionObject
import io.simplifier.pluginbase.util.logging.{DeferredLogging, Logging}

import java.io.File
import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.sys.ShutdownHookThread
import scala.util.{Failure, Success, Try}

/**
  * Main application class for plugins.
  */
trait SimplifierPlugin {

  self: SimplifierPluginLogic =>

}

/**
  * Main application logic for plugins.
  */
abstract class SimplifierPluginLogic(pluginDescription: => PluginDescription,
                                     logfileBaseName: String) extends App with DeferredLogging {

  // This sets the logfile basename, so it must be called before the first Logger is initialized!
  Logging.initializeLoggingProperties(logfileBaseName)

  val pluginSecret:String

  val PLUGIN_CONFIG: Config = getConfig

  implicit val ACTOR_SYSTEM: ActorSystem = ActorSystem("plugin-system", PLUGIN_CONFIG)
  implicit val MATERIALIZER: Materializer = Materializer(ACTOR_SYSTEM)

  RoleCacheActor(ACTOR_SYSTEM)

  import ACTOR_SYSTEM.dispatcher

  lazy val BASIC_STATE: BasicState = getBasicState(pluginSecret)
  var PLUGIN_DESCRIPTION: PluginDescription = pluginDescription
  var SHUTDOWN_TIMEOUT: FiniteDuration = 60.seconds
  var MAXIMUM_TIMEOUT: FiniteDuration = FiniteDuration(180, SECONDS)

  //Level 5: Ready process
  val READY_TO_BE_USED_PROCESS = StatefulProcess.ChildProcess(isReady)

  // Level 4: Registration process
  val REGISTRATION_PROCESS = StatefulProcess.ChildProcess(startRegistration, stopRegistration) withChildProcess READY_TO_BE_USED_PROCESS

  // Level 3: Webservice
  val WEBSERVICE_PROCESS = StatefulProcess.ChildProcess(startWebservice, stopWebService) withChildProcess REGISTRATION_PROCESS

  // Level 2: Plugin service process
  val SERVICE_PROCESS = StatefulProcess.ChildProcess(startService, stopService) withChildProcess WEBSERVICE_PROCESS

  // Level 1: Basic plugin process
  val BASIC_PROCESS = StatefulProcess.ChildProcess(initializePlugin, deInitializePlugin) withChildProcess SERVICE_PROCESS

  // Level 0: Arguments Passing Process
  val ARGUMENTS_PASSING_PROCESS = StatefulProcess(passingArguments _) withChildProcess BASIC_PROCESS

  /*
   *  --- Basic plugin process ---
   */

  /**
    * Pseudo function for the pseudo state <b>ARGUMENTS_PASSING_PROCESS</b> so that the <b>BASIC_PROCESS</b> will have the main arguments available.
    *
    * @return the future for the next process.
    */
  def passingArguments: Future[Array[String]] = Future(args)

  /*
   *  --- Pre Basic plugin process ---
   */


  /**
    * Initializing the plugin. This future will not be resolved here!
    *
    * @return the basic state.
    */
  private def initializePlugin(arguments: Array[String]): Future[BasicState] = {
    logger.info(Action(StartingWithArguments(args)))
    logger.info(Action(LOADING_PLUGIN_DESCRIPTION))
    PLUGIN_DESCRIPTION = PluginDescription.load()
    logger.info(ActionDone(LOADING_PLUGIN_DESCRIPTION_DONE))
    logger.info(PluginAction(STARTING_PLUGIN, PLUGIN_DESCRIPTION))
    logger.info(PluginAction(INITIALIZING_PLUGIN, PLUGIN_DESCRIPTION))
    Try(Await.result(Future(BASIC_STATE), MAXIMUM_TIMEOUT)) match {
      case Success(res) => logger.info(PluginActionDone(INITIALIZING_PLUGIN_DONE, PLUGIN_DESCRIPTION))
        Future(res)
      case Failure(e) => logger.error(ErrorDuringPluginAction(INITIALIZING_PLUGIN_ERROR, e, PLUGIN_DESCRIPTION))
        terminate(Some(e))
        throw e
    }
  }

  /**
    * Deinitializing the plugin.
    *
    * @param basicState the basic state.
    * @return           a Future consisting of nothing.
    */
  private def deInitializePlugin(basicState: BasicState): Future[Unit] = {
    logger.info(PluginAction(DEINITIALIZING_PLUGIN, PLUGIN_DESCRIPTION))
    Future(logger.info(PluginActionDone(DEINITIALIZING_PLUGIN_DONE, PLUGIN_DESCRIPTION)))
  }


  /*
   * ---- Plugin Service process ---
   */

  /**
    * Starting the services of the plugin.
    *
    * @param basicState the previous state (basic).
    * @return           the new service state as a future or an exception.
    */
  private def startService(basicState: BasicState): Future[ServiceState] = {
    logger.info(PluginAction(STARTING_SERVICES, PLUGIN_DESCRIPTION))
    Try(Await.result(startPluginServices(basicState), MAXIMUM_TIMEOUT)) match {
      case Success(baseService) => logger.info(PluginActionDone(STARTING_SERVICES_DONE, PLUGIN_DESCRIPTION))
        Future(ServiceState(basicState, baseService))
      case Failure(e) => logger.error(ErrorDuringPluginAction(STARTING_SERVICES_ERROR, e, PLUGIN_DESCRIPTION))
        terminate(Some(e))
        throw e
    }
  }


  /**
    * Stopping the services of the plugin.
    *
    * @param state  the service state.
    * @return       a Future consisting of nothing.
    */
  private def stopService(state: ServiceState): Future[Unit] = {
    logger.info(PluginAction(STOPPING_SERVICES, PLUGIN_DESCRIPTION))
    Try(Await.result(stopPluginServices(), MAXIMUM_TIMEOUT)) match {
      case Success(_) => Future(logger.info(PluginActionDone(STOPPING_SERVICES_DONE, PLUGIN_DESCRIPTION)))
      case Failure(e) => logger.error(ErrorDuringPluginAction(STOPPING_SERVICES_ERROR, e, PLUGIN_DESCRIPTION))
        terminate(Some(e))
        throw e
    }
  }


  /*
   *  --- Web Service process ---
   */

  /**
    * Starting the webservice of the plugin.
    *
    * @param serviceState the previous state (service).
    * @return             the new webservice state as a future or an exception.
    */
  private def startWebservice(serviceState: ServiceState): Future[WebServiceState] = {
    logger.info(PluginAction(STARTING_WEBSERVICE, PLUGIN_DESCRIPTION))
    val ServiceState(pluginState, baseService) = serviceState
    Try(Await.result(baseService.start(), MAXIMUM_TIMEOUT)) match {
      case Success(dynamicPort) =>
       val pluginName = serviceState.pluginState.pluginDescription.name
        val filePermissions = PluginPermissionObject.readPermissionFile(pluginName)
        val manifest = PluginManifestBuilder(pluginState.pluginDescription, pluginState.settings, baseService,
          dynamicPort, pluginPermissions ++ filePermissions.map(p => Seq(p)).getOrElse(Seq()))
        filePermissions.foreach {_ =>
          logger.info(s"Permission file found for plugin '$pluginName'.")
        }
        implicit val userSession: UserSession = UserSession.unauthenticated
        implicit val requestSource: RequestSource = Plugin(pluginName, None)
        val roleNames = RoleCaches.readRoleNameFile(pluginName).getOrElse(Seq())
        serviceState.pluginState.dispatcher.initRoleCache(roleNames)
        SimplifierServerSettings.initServerSettings(serviceState.pluginState.dispatcher)
        logger.info(PluginActionDone(STARTING_WEBSERVICE_DONE, PLUGIN_DESCRIPTION))
        Future(WebServiceState(pluginState, baseService, manifest))
      case Failure(e) =>
        logger.error(ErrorDuringPluginAction(STARTING_WEBSERVICE_ERROR, e, PLUGIN_DESCRIPTION))
        Try(Await.result(baseService.stop(), MAXIMUM_TIMEOUT)) match {
          case Success(_) => terminate(Some(e))
          case Failure(err) => err.addSuppressed(e)
            terminate(Some(err))
        }
        throw e
    }
  }

  /**
    * Stopping the webservice of the plugin.
    *
    * @param state  the webservice state.
    * @return       a Future consisting of nothing.
    */
  private def stopWebService(state: WebServiceState): Future[Unit] = {
    logger.info(PluginAction(STOPPING_WEBSERVICE, PLUGIN_DESCRIPTION))
    Try(Await.result(state.baseService.stop(), MAXIMUM_TIMEOUT)) match {
      case Success(_) => Future(logger.info(PluginActionDone(STOPPING_WEBSERVICE_DONE, PLUGIN_DESCRIPTION)))
      case Failure(e) => logger.error(ErrorDuringPluginAction(STOPPING_WEBSERVICE_ERROR, e, PLUGIN_DESCRIPTION))
        terminate(Some(e))
        throw e
    }
  }


  /*
   * --- Simplifier registration process ---
   */

  /**
    * Starting the registration process of the plugin.
    *
    * @param serviceState the previous state (webservice).
    * @return             the new registration state as a future or an exception.
    */
  private def startRegistration(serviceState: WebServiceState): Future[RegistrationState] = {
    logger.info(PluginAction(STARTING_REGISTRATION, PLUGIN_DESCRIPTION))
    Try(Await.result(serviceState.pluginState.dispatcher.startDispatcher(serviceState.manifest), MAXIMUM_TIMEOUT)) match {
      case Success(_) => logger.info(PluginActionDone(STARTING_REGISTRATION_DONE, PLUGIN_DESCRIPTION))
        Future(RegistrationState(serviceState.pluginState))
      case Failure(e) => logger.error(ErrorDuringPluginAction(STARTING_REGISTRATION_ERROR, e, PLUGIN_DESCRIPTION))
        terminate(Some(e))
        throw e
    }
  }


  /**
    * Stops the registration process of the plugin.
    *
    * @param state the new registration state.
    * @return      a Future consisting of nothing.
    */
  private def stopRegistration(state: RegistrationState): Future[Unit] = {
    logger.info(PluginAction(STOPPING_REGISTRATION, PLUGIN_DESCRIPTION))
    Try(Await.result(state.pluginState.dispatcher.stopDispatcher(), MAXIMUM_TIMEOUT)) match {
      case Success(_) => Future(logger.info(PluginActionDone(STOPPING_REGISTRATION_DONE, PLUGIN_DESCRIPTION)))
      case Failure(e) => logger.error(ErrorDuringPluginAction(STOPPING_REGISTRATION_ERROR, e, PLUGIN_DESCRIPTION))
        terminate(Some(e))
        throw e
    }
  }


  /*
   * --- Simplifier final plugin state ---
   */

  /**
    * Final state that indicates, that the plugin is ready to be used.
    *
    * @param registrationState the previous state (registration).
    * @return                  nothing, but the logger message, that the plugin is ready to be used.
    */
  private def isReady(registrationState: RegistrationState): Future[Unit] = {
    Try(Await.result(Future(registrationState), MAXIMUM_TIMEOUT)) match {
      case Success(_) =>
        val pluginName = registrationState.pluginState.pluginDescription.name
        val permissionFileOpt = PluginPermissionObject.getPermissionFile(pluginName)
        implicit val userSession: UserSession = UserSession.unauthenticated
        implicit val requestSource: RequestSource = Plugin(pluginName, None)
        permissionFileOpt foreach { permissionFile =>
          logger.info(s"Watching for changes at permission file ${permissionFile.toAbsolutePath.toString}")
          new FileWatch() {
            override def onChange(file: File): Unit = {
              logger.info(s"Change detected at permission file ${permissionFile.toAbsolutePath.toString}.")
              PluginPermissionObject.readPermissionFile(pluginName) foreach { permissionObject =>
                val dispatcher = new AppServerDispatcher(registrationState.pluginState.settings, registrationState.pluginState.pluginDescription)
                dispatcher.addPermissionObjects(Seq(PluginPermissionObject.toApi(permissionObject)))
              }
            }
          }.watchFile(permissionFile.toFile)
        }
        val roleNameFileOpt = RoleCaches.getRoleNameFile(pluginName)
        roleNameFileOpt.foreach { roleNameFile =>
          logger.info(s"Watching for changes at role name file ${roleNameFile.toAbsolutePath.toString}")
          new FileWatch() {
            override def onChange(file: File): Unit = {
              logger.info(s"Change detected at role name file ${roleNameFile.toAbsolutePath.toString}")
              RoleCaches.readRoleNameFile(pluginName) foreach { roleNames =>
                val dispatcher = new AppServerDispatcher(registrationState.pluginState.settings, registrationState.pluginState.pluginDescription)
                import scala.concurrent.ExecutionContext.Implicits.global
                dispatcher.initRoleCache(roleNames)
              }
            }
          }.watchFile(roleNameFile.toFile)
        }
        Future(logger.info(PluginUsable(PLUGIN_DESCRIPTION)))
      case Failure(e) => logger.error(ErrorDuringPluginAction(WAITING_TO_BE_USABLE, e, PLUGIN_DESCRIPTION))
        terminate(Some(e))
        throw e
    }
  }

  /**
    * Returns the basic state of the plugin.
    *
    * @return the basic state of the plugin.
    */
  private def getBasicState(pluginSecret: String): BasicState = {

    logger.info(Action(LOADING_PLUGIN_SETTINGS))
    val settings: PluginSettings = PluginSettings(PLUGIN_CONFIG, pluginSecret)
    MAXIMUM_TIMEOUT = settings.maximumTimeout
    SHUTDOWN_TIMEOUT = settings.shutdownTimeout
    logger.info(ActionDone(LOADING_PLUGIN_SETTINGS_DONE))

    logger.info(Action(ASKING_SIMPLIFIER_INSTANCE_LOCATION))
    val dispatcher = new AppServerDispatcher(settings, PLUGIN_DESCRIPTION)
    val appServerPrefix: Option[String] = Try(Option(Await.result(dispatcher.getAppServerPrefix, Duration(60, TimeUnit.SECONDS)))).getOrElse(None)
    val appServerRoutes: Option[Routes] = Try(Await.result(dispatcher.getRoutes, Duration(60, TimeUnit.SECONDS))).getOrElse(None)
    logger.info(ActionDone(ASKING_SIMPLIFIER_INSTANCE_LOCATION_DONE))

    BasicState(PLUGIN_CONFIG, settings, PLUGIN_DESCRIPTION, dispatcher, AppServerInformation(appServerPrefix, appServerRoutes))
  }

  private def getConfig = {
    val settingsLocation: String = if (!Option(args).exists(_.nonEmpty)) {
      logger.warn(s"Started with empty arguments: ${args.mkString("[",", ","]")}")
      defaultSettingsLocation
    } else {
      logger.info(s"Started with arguments: ${args.mkString("[",", ","]")}")
      args.head
    }
    logger.info(ConfigLoaded(settingsLocation))
    val config: Config = ConfigLoader(args, defaultSettingsLocation).loadConfig()
    logger.info(ConfigLoadedDone(settingsLocation))
    config
  }

  /*
   * --- Abstract function provided by the concrete plugin ---
   */

  protected def defaultSettingsLocation = "settings"

  /**
    * The plugin permissions
    *
    * @return a sequence of the plugin permissions
    */
  def pluginPermissions: Seq[PluginPermissionObject] = Seq.empty

  /**
    * Starts the plugin service
    *
    * @param basicState the basic state containing all relevant information
    * @return Future containing
    */
  def startPluginServices(basicState: BasicState): Future[PluginBaseHttpService] = Future {
    logger.warn(NoBaseServicesProvided(PLUGIN_DESCRIPTION))
    new PluginBaseHttpService(PLUGIN_DESCRIPTION, basicState.settings,basicState.appServerInformation, None, None, None)
  }

  def stopPluginServices(): Future[Unit] = Future.successful(())

  /*
   * --- Plugin process workflow ---
   */

  /**
    * Starts the basic process.
    */
  def run(): Future[Array[String]] = {
    ARGUMENTS_PASSING_PROCESS.startUp()
  }


  /**
    * Terminate plugin process.
    *
    * @param error the optional error, that cause this process.
    * @return      either nothing or an exception which might have happened during the process.
    */
  def terminate(error: Option[Throwable]): Future[Unit] = {
    logger.info(PluginAction(TERMINATING_PLUGIN, PLUGIN_DESCRIPTION))
    ARGUMENTS_PASSING_PROCESS.shutDown() andThen {
      case _ => Try {
        Try(Await.result(ACTOR_SYSTEM.terminate(), SHUTDOWN_TIMEOUT)).get
        logger.info(PluginAction(REMOVING_SHUTDOWN_HOOK, PLUGIN_DESCRIPTION))
        Try(removeShutdownHook())
          .fold(e => logger.error(ErrorDuringPluginAction(REMOVING_SHUTDOWN_HOOK_ERROR, e, PLUGIN_DESCRIPTION), e),
            _ => logger.info(PluginActionDone(REMOVING_SHUTDOWN_HOOK_DONE, PLUGIN_DESCRIPTION)))
        error.foreach(e => logger.error(TerminationCause(e, PLUGIN_DESCRIPTION), e))
      } match {
        case Failure(_: IllegalStateException) => logger.warn(PluginActionAlreadyInProgress(SHUTDOWN, PLUGIN_DESCRIPTION))
        case Failure(e) => logger.error(ErrorDuringPluginAction(TERMINATING_PLUGIN_ERROR, e, PLUGIN_DESCRIPTION), e)
        case Success(_) => logger.info(PluginActionDone(TERMINATING_PLUGIN_DONE, PLUGIN_DESCRIPTION))
      }
    }
  }

  /**
    * Return future for the completed startup.
    * Calling this function does not trigger the startup itself.
    *
    * @return future which is completed, when the startup is either completed or failed
    */
  def awaitStartup(): Future[Unit] = {
    for {
      _ <- ARGUMENTS_PASSING_PROCESS.awaitStartup()
      _ <- BASIC_PROCESS.awaitStartup()
      _ <- SERVICE_PROCESS.awaitStartup()
      _ <- WEBSERVICE_PROCESS.awaitStartup()
      _ <- REGISTRATION_PROCESS.awaitStartup()
      _ <- READY_TO_BE_USED_PROCESS.awaitStartup()
    } yield {
      Unit
    }
  }

  /**
    * Terminate plugin process by system shutdown.
    */
  def terminateBySystemShutdown(): Unit = {

    // Wait for termination to finish (wait until all bindings are unbound etc.)
    logger.info(TerminatingPluginBySystemShutdown(PLUGIN_DESCRIPTION))
    Await.ready(terminate(None), SHUTDOWN_TIMEOUT)
    logger.info(TerminatedPluginBySystemShutdown(PLUGIN_DESCRIPTION))
  }

  // Attach terminate function to vm shutdown
  val SHUTDOWN_HOOK_THREAD: ShutdownHookThread = sys.addShutdownHook(terminateBySystemShutdown())
  protected def removeShutdownHook(): Unit = SHUTDOWN_HOOK_THREAD.remove()

  run()

}


object SimplifierPlugin {

  case class BasicState(config: Config,
                        settings: PluginSettings,
                        pluginDescription: PluginDescription,
                        dispatcher: AppServerDispatcher,
                        appServerInformation: AppServerInformation)

  case class ServiceState(pluginState: BasicState, baseService: PluginBaseHttpService)

  case class WebServiceState(pluginState: BasicState, baseService: PluginBaseHttpService, manifest: PluginManifest)

  case class RegistrationState(pluginState: BasicState)

  case class AppServerInformation(prefix: Option[String], routes: Option[Routes])

}