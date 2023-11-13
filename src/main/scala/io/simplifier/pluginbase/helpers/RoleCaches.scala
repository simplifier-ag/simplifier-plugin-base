package io.simplifier.pluginbase.helpers

import akka.util.Timeout
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginapi.{GrantedPermission, UserSession}
import io.simplifier.pluginbase.helpers.RoleCacheActor._
import io.simplifier.pluginbase.interfaces.AppServerDispatcher
import io.simplifier.pluginbase.util.logging.Logging
import org.json4s._

import java.nio.file.{FileSystems, Files, Path, Paths}
import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object RoleCaches extends Logging {

  import akka.pattern.ask
  private implicit val timeout: Timeout = Timeout(1.minute)

  def addRolesOfInterest(roles: Set[String])
                        (implicit ec: ExecutionContext, userSession: UserSession, requestSource: RequestSource, appServerDispatcher: AppServerDispatcher): Future[Boolean] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? UpdateRoles(roles, appServerDispatcher, userSession, requestSource)
    }.mapTo[UpdateState].map(_.toBoolean)
  }

  def updateUser(userId: Long, roles: Seq[String])(implicit ec: ExecutionContext): Future[Boolean] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? SetRolesForUser(userId, roles)
    }.mapTo[UpdateState].map(_.toBoolean)
  }

  def updateApp(app: String, roles: Seq[String])(implicit ec: ExecutionContext): Future[Boolean] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? SetRolesForApp(app, roles)
    }.mapTo[UpdateState].map(_.toBoolean)
  }

  def permissionsForUser(userId: Long)(implicit ec: ExecutionContext): Future[Option[Seq[GrantedPermission]]] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? LookUpPermissionsForUser(userId)
    }.mapTo[PermissionsForUser].map(_.permissions)
  }

  def permissionsForApp(app: String)(implicit ec: ExecutionContext): Future[Option[Seq[GrantedPermission]]] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? LookUpPermissionsForApp(app)
    }.mapTo[PermissionsForApp].map(_.permissions)
  }

  def permissionsForRoles(roles: Seq[String])(implicit ec: ExecutionContext): Future[Seq[GrantedPermission]] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? LookUpPermissionsForRoles(roles)
    }.mapTo[PermissionsForRoles].map(_.permissions)
  }

  def rolesForUser(userId: Long)(implicit ec: ExecutionContext): Future[Option[Set[String]]] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? LookUpRolesForUser(userId)
    }.mapTo[Roles].map(_.roleNames)
  }

  def rolesForApp(app: String)(implicit ec: ExecutionContext): Future[Option[Set[String]]] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? LookUpRolesForApp(app)
    }.mapTo[Roles].map(_.roleNames)
  }

  def cachedRoles(implicit ec: ExecutionContext): Future[Seq[String]] = {
    RoleCacheActor.roleCacheActor.flatMap { roleCache =>
      roleCache ? LookUpCachedRoles
    }.mapTo[CachedRoles].map(_.roles)
  }

  def getRoleNameFile(pluginName: String): Option[Path] = {
    val path = s"/opt/simplifier/data/plugins/${pluginName}.roles"
    if(Files.exists(Paths.get(path)))
      Some(FileSystems.getDefault.getPath(path))
    else
      None
  }

  def readRoleNameFile(pluginName: String): Option[Seq[String]] = {

    import scala.collection.JavaConverters._
    implicit val formats: Formats = DefaultFormats

    Try {
      getRoleNameFile(pluginName) match {
        case Some(pathToPermissionFile) =>
          (Try(Files.readAllLines(pathToPermissionFile)) match {
            case Success(v) => Some(v.asScala)
            case Failure(e) =>
              logger.error(s"Error during processing plugin permission file: ${pathToPermissionFile.toAbsolutePath}", e)
              None
          }) map { fileContent =>
            fileContent.mkString.split(',').toList.map(_.trim)
          }
        case None =>
          None
      }
    } match {
      case Success(v) => v
      case Failure(e) =>
        logger.error("Error while processing permission file.", e)
        None
    }
  }

}
