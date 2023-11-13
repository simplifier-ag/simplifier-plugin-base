package io.simplifier.pluginbase.interfaces

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.client.RequestBuilding._
import akka.http.scaladsl.model._
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, Unmarshaller}
import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import io.simplifier.pluginapi.RegistrationData.{PermissionObjects, PluginManifest}
import io.simplifier.pluginapi.{GrantedPermissions, PermissionObjectDefinition, RoleNames, RoleNamesRequest, UserOrAppName, UserSession}
import io.simplifier.pluginapi.rest.PluginHeaders.{AppName, ClientBusinessObjectFunctionName, ClientBusinessObjectName, IsInternalUser, JobName, ModuleInterfaceName, ModuleName, ParentPerformanceId, Plugin, RequestSource, RequestSourcePlugin, SimplifierToken, `Plugin-Request-Source`, `Plugin-Secret`, `Simplifier-User`}
import io.simplifier.pluginapi.rest.PluginRequestTransformers
import io.simplifier.pluginapi.role.RoleListWithPermissions
import io.simplifier.pluginapi.serverSettings.ReadServerSettings.{ServerSettingsRequest, Settings}
import io.simplifier.pluginbase.helpers.RoleCaches
import io.simplifier.pluginbase.interfaces.AppServerDispatcher.{DispatcherState, HttpRequestException, Routes, renderEntity}
import io.simplifier.pluginbase.util.io.StreamUtils
import io.simplifier.pluginbase.util.json.JSONFormatter._
import io.simplifier.pluginbase.util.json.SimplifierFormats
import io.simplifier.pluginbase.util.logging.Logging
import io.simplifier.pluginbase.{PluginDescription, PluginSettings, StatefulProcess}
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, Extraction, Formats}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.Try


/**
  * Dispatcher for messages sent to app server.
  *
  * @param pluginSettings plugin settings
  */
class AppServerDispatcher(pluginSettings: PluginSettings, pluginDescription: PluginDescription)
                         (implicit system: ActorSystem, materializer: Materializer) extends Logging with SimplifierFormats with PluginRequestTransformers {

  import system.dispatcher

  /*
   * --- Registration Process ---
   */

  private val registrationProcess = StatefulProcess.ChildProcess(addRegistration, removeRegistration)

  private def addRegistration(pluginManifest: PluginManifest): Future[DispatcherState] = {
    implicit val formats:Formats = DefaultFormats
    logger.info(s"Running application server registration for plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}}.")
    logger.trace("Manifest: \n" + pretty(render(Extraction.decompose(pluginManifest))))
    register(pluginManifest) map {
      _ => DispatcherState(pluginManifest)
    }
  }

  private def removeRegistration(state: DispatcherState): Future[Unit] = {
    logger.info(s"Removing application server registration for plugin: {${state.manifest.plugin.name}} in version: {${state.manifest.plugin.version}}.")
    unregister()
  }

  /**
    * Start dispatcher and registrate on AppServer.
    *
    * @param pluginManifest plugin manifest
    * @return process future
    */
  def startDispatcher(pluginManifest: PluginManifest): Future[Unit] = {
    registrationProcess.startUp(pluginManifest) map { _ => }
  }

  /**
    * Stop dispatcher and unregister on AppServer.
    *
    * @return process future
    */
  def stopDispatcher(): Future[Unit] = registrationProcess.shutDown()

  /*
   * --- Dispatching ---
   */

  /**
    * Send a request to the appServer
    *
    * @param request     the request
    * @param forceStrict default = true
    * @param ec          the execution context
    * @param userSession the user session
    * @return
    */
  def sendViaSimplifierFlow(request: HttpRequest, forceStrict: Boolean = true)
                           (implicit ec: ExecutionContext, userSession: UserSession,
                            requestSource: RequestSource): Future[HttpResponse] = {
    val r = addPluginHeaders(userSession, requestSource)(request).withUri(request.uri.resolvedAgainst(Uri(s"http://${pluginSettings.simplifierHost}:${pluginSettings.simplifierPort}")))
    Http(system).singleRequest(r).flatMap(transformResponse(forceStrict))
  }

  protected def addPluginHeaders(userSession: UserSession, requestSource: RequestSource)(request: HttpRequest): HttpRequest = {
    val performanceLogging = userSession.performanceLoggingData
    request ~>
      `Plugin-Secret`(pluginSettings.pluginSecret) ~>
      `Plugin-Request-Source`(RequestSourcePlugin(pluginDescription.name)) ~>
      addOptionalHeader(userSession.userIdOpt, new `Simplifier-User`(_)) ~>
      addOptionalHeader(userSession.tokenOpt, SimplifierToken(_)) ~>
      addOptionalHeader(userSession.appNameOpt, AppName(_)) ~>
      addOptionalHeader(performanceLogging.flatMap(_.jobName), JobName(_)) ~>
      addOptionalHeader(performanceLogging.flatMap(_.parentPerformanceId), ParentPerformanceId(_)) ~>
      addOptionalHeader(performanceLogging.flatMap(_.moduleName), ModuleName(_)) ~>
      addOptionalHeader(performanceLogging.flatMap(_.moduleInterfaceName), ModuleInterfaceName(_)) ~>
      addOptionalHeader(performanceLogging.flatMap(_.clientBusinessObjectName), ClientBusinessObjectName(_)) ~>
      addOptionalHeader(performanceLogging.flatMap(_.clientBusinessObjectFunctionName), ClientBusinessObjectFunctionName(_)) ~>
      IsInternalUser(userSession.isInternalUser) ~>
      addSimplifierCallUriHeader(requestSource.getUri)
  }

  private def addOptionalHeader[T](value: Option[T], headerGenerator: T => HttpHeader): RequestTransformer = {
    value match {
      case None => identity[HttpRequest]
      case Some(hdrValue) => headerGenerator(hdrValue)
    }
  }

  protected def transformResponse(forceStrict: Boolean)(response: HttpResponse): Future[HttpResponse] = {
    if (forceStrict) {
      for {
        strictEntity <- makeResponseEntityStrict(response)
      } yield {
        response.withEntity(strictEntity)
      }
    } else {
      Future.successful(response)
    }
  }

  /**
    * Force the consumption of the response entity, because otherwise Akka Http will back-pressure the data in the connection.
    *
    * @param response response object
    * @return future of consumed entity data converted to strict
    */
  private def makeResponseEntityStrict(response: HttpResponse): Future[HttpEntity.Strict] = {
    if (response.status.allowsEntity) {
      response.entity.toStrict(pluginSettings.timeoutDuration)
    } else {
      // It is required to drain the entity anyways
      response.discardEntityBytes()
      Future.successful(HttpEntity.Empty)
    }
  }

  /**
    * Extract entity as concrete type.
    *
    * @param entity entity to extract
    * @param um     unmarshaller
    * @tparam A entity type
    * @return future of the decoded entity
    */
  def extractEntity[A](entity: HttpEntity)(implicit um: FromEntityUnmarshaller[A]): Future[A] = {
    um(entity)
  }

  /*
   * --- API ---
   */

  protected def register(manifest: PluginManifest, tryUnregister: Boolean = true): Future[Unit] = {
    implicit val userSession: UserSession = UserSession.unauthenticated
    implicit val requestSource: RequestSource = Plugin(manifest.plugin.name, None)
    val req = Post("/api/registration", manifest)
    sendViaSimplifierFlow(req) map {
      case response if response.status.isSuccess() =>
        // Successful, Nothing to do
        logger.info(s"Plugin: {${manifest.plugin.name}} in version: {${manifest.plugin.version}} registered successfully.")
      case failedResponse if failedResponse.status == StatusCodes.Conflict && tryUnregister =>
        // Registration failed, because old version of plugin is still registered - try to unregister once
        logger.info(s"Plugin: {${manifest.plugin.name}} in version: {${manifest.plugin.version}} already registered - trying to refresh registration with new manifest.")
        unregister() flatMap {
          _ => register(manifest, tryUnregister = false /* only try to unregister once */)
        }
      case failedResponse =>
        // Failed for other reason
        throw HttpRequestException(s"Plugin Registration of plugin: {${manifest.plugin.name}} in version: {${manifest.plugin.version}} failed.", failedResponse)
    }
  }

  protected def unregister(): Future[Unit] = {
    implicit val userSession: UserSession = UserSession.unauthenticated
    implicit val requestSource: RequestSource = Plugin(pluginDescription.name, None)
    val req = Delete(s"/api/registration/${pluginDescription.name}")
    sendViaSimplifierFlow(req) map {
      case response if response.status.isSuccess() =>
        logger.info(s"Plugin: {${pluginDescription.name}} in version: {${pluginDescription.version}} unregistered successfully.")
      case failedResponse =>
        throw HttpRequestException(s"Plugin: {${pluginDescription.name}] in version: {${pluginDescription.version}} failed to unregister.", failedResponse)
    }
  }

  /**
    * Get granted permissions for a user.
    *
    * @param permissionObjectName (optional) technical name of permission object to filter
    * @return
    */
  def getPermissions(permissionObjectName: Option[String] = None)
                    (implicit userSession: UserSession, requestSource: RequestSource): Future[GrantedPermissions] = {
    val uri = permissionObjectName match {
      case Some(poName) =>
        logger.trace(s"Load permissions for $traceTarget and permission object $poName")
        s"/api/permission/$poName"
      case None =>
        logger.trace(s"Load permissions for user $traceTarget")
        s"/api/permission"
    }
    val req = Get(uri)
    sendViaSimplifierFlow(req) flatMap {
      case response if response.status.isSuccess() =>
        extractEntity[GrantedPermissions](response.entity)
      case failedResponse =>
        logger.error("Error loading permissions: " + failedResponse.status)
        throw HttpRequestException("Error loading permissions", failedResponse)
    }
  }

  /**
    * Gets the name of the logged in user or app
    *
    * @param userSession the implicit user session of the currently logged in user
    * @return the name of the logged in user or app
    */
  def getUserOrAppName(implicit userSession: UserSession, requestSource: RequestSource): Future[UserOrAppName] = {
    val req = Get("/api/permission/userOrAppName")
    sendViaSimplifierFlow(req) flatMap {
      case response if response.status.isSuccess() =>
        extractEntity[UserOrAppName](response.entity)
      case failedResponse =>
        logger.error("Error loading user or app name: " + failedResponse.status)
        throw HttpRequestException("Error loading user or app name", failedResponse)
    }
  }

  /**
    * Gets the names of all roles assigned to the logged in user
    *
    * @param userSession the implicit user session of the currently logged in user
    * @return the names of the roles assigned to the logged in user
    */
  def getAllRoleNames(implicit userSession: UserSession, requestSource: RequestSource): Future[Set[String]] = {
    def getRoleNamesForUser(userId: Long)(implicit userSession: UserSession, requestSource: RequestSource): Future[Set[String]] = {
      for {
        roleNames <- getAllRoleNamesFromAppServer
        _ <- RoleCaches.updateUser(userId, roleNames.roleNames.toSeq)
      } yield roleNames.roleNames
    }

    def getRoleNamesForApp(appName: String)(implicit userSession: UserSession, requestSource: RequestSource): Future[Set[String]] = {
      for {
        roleNames <- getAllRoleNamesFromAppServer
        _ <- RoleCaches.updateApp(appName, roleNames.roleNames.toSeq)
      } yield roleNames.roleNames
    }

    (userSession.userIdOpt, userSession.appNameOpt) match {
      case (Some(userId), _) =>
        RoleCaches.rolesForUser(userId).flatMap {
          case Some(roles) => Future.successful(roles)
          case None => getRoleNamesForUser(userId)
        }
      case (_, Some(appName)) =>
        RoleCaches.rolesForApp(appName).flatMap {
          case Some(roles) => Future.successful(roles)
          case None => getRoleNamesForApp(appName)
        }
      case _ => Future.failed(new Exception("no User info was provided"))
    }
  }

  private def getAllRoleNamesFromAppServer(implicit userSession: UserSession, requestSource: RequestSource): Future[RoleNames] = {
    val req = Get("/api/permission/roles")
    sendViaSimplifierFlow(req) flatMap {
      case response if response.status.isSuccess() =>
        extractEntity[RoleNames](response.entity)
      case failedResponse =>
        logger.error("Error loading role names: " + failedResponse.status)
        throw HttpRequestException("Error loading role names", failedResponse)
    }
  }

  /**
    * Gets either all or only the given roles with their permissions
    *
    * @param roleNames the roles to be fetched. If none are given all roles are fetched
    * @param userSession
    * @param requestSource
    */
  def getAllRolesWithPermission(roleNames: Seq[String])(implicit userSession: UserSession, requestSource: RequestSource): Future[RoleListWithPermissions.success] = {
    val req = Post("/api/slot/listRolesWithPermission", RoleNamesRequest(pluginDescription.name, roleNames.toSet))
    sendViaSimplifierFlow(req) flatMap {
      case response if response.status.isSuccess() =>
        extractEntity[RoleListWithPermissions.success](response.entity)
      case failedResponse =>
        logger.error("Error loading role names: " + failedResponse.status)
        throw HttpRequestException("Error loading role names", failedResponse)
    }
  }

  def addPermissionObjects(permissionObjects: Seq[PermissionObjectDefinition])(implicit userSession: UserSession, requestSource: RequestSource): Unit = {
    val req = Post("/api/permission/addPermissionObjects", PermissionObjects(Some(permissionObjects)))
    sendViaSimplifierFlow(req) flatMap {
      case response if response.status.isSuccess() => Future.successful(())
      case e =>
        logger.error("Error adding permission objects.")
        Future.failed(throw HttpRequestException("Error adding permission objects.", e))
    }
  }

  private def traceTarget(implicit userSession: UserSession): String = {
    userSession.tokenOpt.fold {
      userSession.userIdOpt.fold {
        "unauthenticated user"
      }(userId => s"user $userId")
    }(token => s"token $token")
  }

  /**
    * Download Upload/Asset from App Server.
    * If the result future resolves successfully, the contained stream must be drained in order to
    * free the connection to the app server.
    *
    * @param assetId sessionId / uploadId / assetId
    * @return future of asset source
    */
  def downloadAsset(assetId: String)
                   (implicit userSession: UserSession, requestSource: RequestSource): Future[Source[ByteString, Any]] = {
    val req = Get(s"/api/asset/$assetId?chunked=true")
    sendViaSimplifierFlow(req, forceStrict = false) map {
      case response if response.status.isSuccess() =>
        response.entity.withoutSizeLimit.dataBytes
      case failedResponse =>
        logger.error("Error fetching asset: " + failedResponse.status)
        throw HttpRequestException("Error fetching asset", failedResponse)
    }
  }

  /**
    * Download Upload/Asset from App Server.
    *
    * @param assetId sessionId / uploadId / assetId
    * @return future of asset as Byte Array
    */
  def downloadAssetAsByteArray(assetId: String)
                              (implicit userSession: UserSession, requestSource: RequestSource): Future[Array[Byte]] = {
    downloadAsset(assetId) flatMap StreamUtils.foldToByteArray
  }

  def getAppServerPrefix: Future[String] = {
    implicit val userSession: UserSession = UserSession.unauthenticated
    implicit val requestSource: RequestSource = Plugin(pluginDescription.name, None)
    val req = Get("/api/information/appServerPrefix")
    sendViaSimplifierFlow(req) map {
      case response if response.status.isSuccess() =>
        Try(Await.result(response.entity.toStrict(60.seconds).map(_.data.utf8String), 60.seconds))
          .fold(e => {
            logger.error(s"An ${e.getClass.getName} occurred during the fetching of the app server prefix!")
            throw new HttpRequestException(s"${e.getClass.getName} during the the fetching of the app server prefix!",
              StatusCodes.InternalServerError.intValue, renderEntity(response))
          }, res => res)
      case failedResponse =>
        logger.error("Error while fetching of the app server prefix: " + failedResponse.status)
        throw HttpRequestException("Error while fetching the app server prefix ", failedResponse)
    }
  }


  def getRoutes: Future[Option[Routes]] = {
    implicit val userSession: UserSession = UserSession.unauthenticated
    implicit val requestSource: RequestSource = Plugin(pluginDescription.name, None)
    val req = Get("/api/information/routes")
    sendViaSimplifierFlow(req) map {
      case response if response.status.isSuccess() =>
        Try(Extraction.extractOpt[Routes](parseJSON(Await.result(response.entity.toStrict(60.seconds).map(_.data.toArray), 60.seconds)).get))
          .fold(e => {
            logger.error(s"An ${e.getClass.getName} occurred during the parsing and extraction of the fetched app server routes!")
            throw new HttpRequestException(s"${e.getClass.getName} during the the parsing and extraction of the fetched app server routes!",
              StatusCodes.InternalServerError.intValue, renderEntity(response))
          }, res => res)
      case failedResponse =>
        logger.error("Error while fetching of the app server routes: " + failedResponse.status)
        throw HttpRequestException("Error while fetching app server routes ", failedResponse)
    }
  }

  /**
    * Initialises the role cache with either all roles if no role names are given or only the given roles
    *
    * @param roleNames     the names of the roles to be initialised for the plugin
    * @param userSession   the implicit user session of the logged in user
    * @param requestSource the request source
    */
  def initRoleCache(roleNames: Seq[String])(implicit userSession: UserSession, requestSource: RequestSource, executionContext: ExecutionContext): Future[Boolean] = {
    implicit val dispatcher: AppServerDispatcher = this
    RoleCaches.addRolesOfInterest(roleNames.toSet)
  }

  /**
    * Requests server settings from the appserver containing the info about the extended permission check
    *
    * @param userSession   the implicit user session of the logged in user
    * @param requestSource the request source
    * @return the server settings received from the appserver
    */
  def getEnablePluginPermissionCheck()(implicit userSession: UserSession, requestSource: RequestSource): Future[Settings] = {
    val req = Post("/api/slot/getServerSettings", ServerSettingsRequest())
    sendViaSimplifierFlow(req) flatMap {
      case response if response.status.isSuccess() =>
        logger.trace(s"Response Entity ${response.entity}")
        extractEntity[Settings](response.entity)
      case failedResponse =>
        logger.error("Error loading server settings: " + failedResponse.status)
        throw HttpRequestException("Error loading server settings", failedResponse)
    }
  }
}

object AppServerDispatcher {

  case class DispatcherState(manifest: PluginManifest)

  case class Routes(client: Map[String, String])

  case class HttpRequestException(msg: String, status: Int, body: String) extends RuntimeException(msg + s" (Code $status):\n" + body)

  object HttpRequestException {

    def apply(msg: String, resp: HttpResponse): HttpRequestException =
      HttpRequestException(msg, resp.status.intValue, renderEntity(resp))

  }

  case class DispatchingException(msg: String) extends RuntimeException(msg)

  /**
    * Render http entity as String by best matching charset.
    * The response entity is required to be strict, which is guaranteed, if the response came through the Simplifier flow.
    *
    * @param resp response, containing http entity
    * @return rendered content as String
    */
  def renderEntity(resp: HttpResponse): String = {
    resp.entity match {
      case strict: HttpEntity.Strict => renderEntity(strict)
      case other =>
        //resp.discardEntityBytes()
        // TODO: implicit materializer
        "[chunked content]"
    }
  }

  /**
    * Render http entity as String by best matching charset.
    *
    * @param entity http entity to render
    * @return rendered content as String
    */
  def renderEntity(entity: HttpEntity.Strict): String = {
    val charset = Unmarshaller.bestUnmarshallingCharsetFor(entity)
    if (entity.contentType.binary) {
      entity.toString // renders part of binary
    } else {
      entity.data.decodeString(charset.nioCharset)
    }
  }

}