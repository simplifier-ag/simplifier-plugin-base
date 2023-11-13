package io.simplifier.pluginbase.interfaces

import akka.http.scaladsl.model.StatusCodes.Forbidden
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import io.simplifier.pluginbase.PluginDescription

import scala.concurrent.ExecutionContext.Implicits.global
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginbase.helpers.{RoleCaches, SimplifierServerSettings}
import io.simplifier.pluginbase.permission.{PluginPermissionFile, PluginPermissionObject}
import io.simplifier.pluginbase.slotservice.Constants.{ACTION_GET, ACTION_UPDATE, PERMISSION_FILE}
import io.simplifier.pluginbase.slotservice.{Constants, GenericSlotService, RestMessages}
import io.simplifier.pluginbase.slotservice.GenericFailureHandling.OperationFailureMessage
import io.simplifier.pluginbase.util.api.{ApiMessage, PredefinedApiFailures, StreamApiMessage}
import io.simplifier.pluginbase.util.json.SimplifierFormats
import io.simplifier.pluginbase.util.logging.Logging
import io.simplifier.pluginbase.permission.PluginPermissionFile
import io.simplifier.pluginbase.util.api.{ApiMessage, StreamApiMessage}
import io.swagger.annotations.{Api, ApiOperation, ApiParam, ApiResponse, ApiResponses, Authorization}
import org.json4s.Extraction
import org.json4s.jackson.JsonMethods._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import javax.ws.rs.Path
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

/**
  * Prototype for an HTTP service providing plugin slots
  */
abstract class SlotInterfaceService(dispatcher: AppServerDispatcher, pluginDescription: PluginDescription,
                                    pluginPermission: PluginPermissionObject)
  extends GenericSlotService with Logging with SimplifierFormats {

  import SlotInterfaceService._

  /** Base-URL relative to http service root */
  val baseUrl: String

  def pluginSlotNames: Seq[String]

  private val systemSlotNames: Seq[String] = Seq("ping", "uploadPermissionFile", "downloadPermissionFile", "clearCaches",
    "caches", "refreshServerSettings")

  /** Available slot names */
  lazy val slotNames: Seq[String] = pluginSlotNames ++ systemSlotNames

  /** Set of names of Slots, which are only allowed to be executed in Standalone (=No Cluster) Mode or on the Primary Cluster server */
  def standaloneOnlySlots: Option[Set[String]] = None

  /**
    * Plugin-specific inner route handling slot requests
    *
    * @param requestSource plugin request source
    * @param userSession   authenticated user session
    * @return service route
    */
  def serviceRoute(implicit requestSource: RequestSource, userSession: UserSession): Route

  private def uploadPermissionFile(permissionFile: PluginPermissionFile)
                                  (implicit requestSource: RequestSource, userSession: UserSession): Future[ApiMessage] = {
    def noPermissionException = throw OperationFailureMessage("No permission to upload permission file", Constants.ERROR_CODE_MISSING_PERMISSION)
    //If / else not working because check for user permission is a future.
    isUserPermittedToAccessPermissionFile.map {
      case true =>
        val json = pretty(render(Extraction.decompose(permissionFile)))
        val filePath = PluginPermissionObject.getPermissionFilePath(pluginDescription.name)
        val file = Paths.get(filePath).toFile
        file.getParentFile.mkdirs()
        file.createNewFile()
        Files.write(file.toPath, json.getBytes(StandardCharsets.UTF_8))
        UploadPermissionFileResponse()
      case false => noPermissionException
    }
  }

  private def downloadPermissionFile(request: DownloadPermissionFileRequest)
                                    (implicit requestSource: RequestSource, userSession: UserSession): Future[StreamApiMessage] = {
    def noPermissionException = throw OperationFailureMessage("No permission to download permission file", Constants.ERROR_CODE_MISSING_PERMISSION)

    isUserPermittedToAccessPermissionFile.map {
      case true =>
        val filePath = PluginPermissionObject.getPermissionFilePath(pluginDescription.name)
        val file = Paths.get(filePath)
        Try {
          Files.readAllBytes(file).grouped(1000)
        } match {
          case Success(bytes) => DownloadPermissionFileResponse(bytes)
          case Failure(ex) =>
            logger.debug(ex.getMessage)
            DownloadPermissionFileResponse(Iterator.empty)
        }
      case false => noPermissionException
    }
  }

  private def isUserPermittedToAccessPermissionFile()(implicit requestSource: RequestSource, userSession: UserSession): Future[Boolean] = {
    dispatcher.getPermissions(Some(pluginPermission.technicalName)).map { permissions =>
      permissions.permissionObjects.headOption.exists { permission =>
        permission.characteristics.exists {
          case (characteristicName, characteristicValues) =>
            characteristicName == PluginPermissionObject.characteristicManagePermissions && characteristicValues.headOption.contains("true")
        }
      }
    }
  }

  /**
    * Needs to be overwritten in each plugin that wants to do additional permission checks
    *
    * @param userSession   the implicit user session of the currently logged in user
    * @param requestSource the request source
    */
  protected def checkAdministratePermission()(implicit userSession: UserSession, requestSource: RequestSource): Unit = {}

  private def checkAdministratePermissionIfNotInternal()(implicit userSession: UserSession, requestSource: RequestSource): Unit = {
    if (!userSession.isInternalUser) checkAdministratePermission()
  }

  /**
    * The base route of this service
    *
    * @param requestSource the implicit request source.
    * @param userSession   the implicit user session.
    * @return the base route.
    */
  def route(implicit requestSource: RequestSource, userSession: UserSession): Route = {
    path("ping") {
      post {
        complete("pong")
      }
    } ~
      (if (userSession.userIdOpt.isEmpty && userSession.tokenOpt.isEmpty) {
        complete(Forbidden -> "Only authenticated access permitted")
      } else {
        path("clearCaches") {
          post {
            onComplete(resetCaches) { _ =>
              complete(CacheResponse("caches cleared and reinitialised"))
            }
          }
        } ~
          path("caches") {
            post {
              extractExecutionContext { implicit ec =>
                checkAdministratePermissionIfNotInternal()
                onComplete(RoleCaches.cachedRoles) {
                  case Success(roles) =>
                    complete(CacheResponse(roles.mkString(", ")))
                  case Failure(exception) =>
                    complete(PredefinedApiFailures.UnexpectedErrorFailure(exception).asResponse)
                }
              }
            }
          } ~
          path("uploadPermissionFile") {
            asyncRequestHandler(uploadPermissionFile, RestMessages.uploadPermissionFileError, ACTION_UPDATE, PERMISSION_FILE)
          } ~
          path("downloadPermissionFile") {
            streamRequestHandler(downloadPermissionFile, RestMessages.downloadPermissionFileError, ACTION_GET, PERMISSION_FILE)
          } ~
          path("refreshServerSettings") {
            post {
              Try(checkAdministratePermissionIfNotInternal()) match {
                case Success(_) => complete(RefreshSettingsResponse(SimplifierServerSettings.reFresh))
                case Failure(e) => complete(PredefinedApiFailures.UnexpectedErrorFailure(e.getMessage, e).asResponse)
              }
            }
          } ~
          post {
            serviceRoute
          }
      })
  }

  private def resetCaches(implicit userSession: UserSession, requestSource: RequestSource): Future[Unit] = {
    checkAdministratePermissionIfNotInternal()
    val pluginName = pluginDescription.name
    val roleNames = RoleCaches.readRoleNameFile(pluginName).getOrElse(Seq())
    dispatcher.initRoleCache(roleNames).mapTo[Unit]
  }

}

object SlotInterfaceService {
  case class DownloadPermissionFileRequest() extends ApiMessage

  case class DownloadPermissionFileResponse(data: Iterator[Array[Byte]], msg: Option[String] = None) extends StreamApiMessage

  case class UploadPermissionFileResponse(msg: String = "Permission file successfully uploaded") extends ApiMessage

  case class CacheResponse(msg: String) extends ApiMessage

  case class RefreshSettingsResponse(msg: String) extends ApiMessage

  @Api(tags = Array("Permission file"), authorizations = Array(new Authorization("basicAuth")))
  @ApiResponses(Array(
    new ApiResponse(code = 401, message = "Unauthorized")
  ))
  @Path("/client/2.0/pluginSlot/")
  trait Documentation {
    @ApiOperation(httpMethod = "POST", value = "Download the permission file.")
    @Path("/downloadPermissionFile")
    protected def downloadPermissionFile(@ApiParam(required = true) request: DownloadPermissionFileRequest): DownloadPermissionFileResponse

    @ApiOperation(httpMethod = "POST", value = "Upload the permission file.")
    @Path("/uploadPermissionFile")
    protected def uploadPermissionFile(@ApiParam(required = true) request: PluginPermissionFile): UploadPermissionFileResponse
  }
}