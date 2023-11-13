package io.simplifier.pluginbase.helpers

import akka.actor.{Actor, ActorRef, ActorSystem, Props, ReceiveTimeout, Stash}
import akka.pattern.pipe
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginapi.role.RoleListWithPermissions.{MinimalPermissionInfo, RoleWithPermissions}
import io.simplifier.pluginapi.{GrantedPermission, UserSession}
import io.simplifier.pluginbase.helpers.RoleCacheActor._
import io.simplifier.pluginbase.interfaces.AppServerDispatcher
import io.simplifier.pluginbase.util.logging.Logging

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.{Duration, FiniteDuration}
import scala.concurrent.{Future, Promise}

class RoleCacheActor extends Actor with Stash with Logging {

  import context.dispatcher

  val receiveTimeoutClearingCaches: FiniteDuration = FiniteDuration(10, TimeUnit.SECONDS)

  override def receive: Receive = {
    context.setReceiveTimeout(receiveTimeoutClearingCaches)
    clearingCaches(Caches())
  }

  def lookingUp(caches: Caches): Receive = {
    case updateRoles@UpdateRoles(_, appServerDispatcher, userSession, requestSource) =>
      logger.trace("Update roles")
      sender() ! Changed
      val newCaches = caches + updateRoles
      getRolesFromSimplifier(newCaches) pipeTo self
      unstashAll()
      context.setReceiveTimeout(receiveTimeoutClearingCaches)
      context.become(clearingCaches(newCaches))
    case LookUpCachedRoles =>
      logger.trace("Lookup cache roles")
      sender() ! caches.roles
    case LookUpRolesForUser(userId) =>
      logger.trace("Lookup roles for user")
      sender() ! caches.rolesOfUser(userId)
    case LookUpRolesForApp(app) =>
      logger.trace("Lookup roles for app")
      sender() ! caches.rolesOfApp(app)
    case LookUpPermissionsForRoles(roles) =>
      logger.trace("Lookup permissions for roles")
      sender() ! caches.permissionsOfRoles(roles)
    case LookUpPermissionsForUser(userId) if !caches.userCache.contains(userId) =>
      logger.trace("Lookup permissions for user. Cache miss.")
      sender() ! PermissionsForUser(userId, None)
    case LookUpPermissionsForUser(userId) =>
      logger.trace("Lookup permissions for user. Cache hit.")
      sender() ! caches.permissionsOfUser(userId)
    case LookUpPermissionsForApp(app) if !caches.appCache.contains(app) =>
      logger.trace("Lookup permissions for app. Cache miss")
      sender() ! PermissionsForApp(app, None)
    case LookUpPermissionsForApp(app) =>
      logger.trace("Lookup permissions for app. Cache hit.")
      sender() ! caches.permissionsOfApp(app)
    case rolesOfUser: SetRolesForUser =>
      logger.trace("Set roles for user")
      sender() ! Changed
      context.become(lookingUp(caches + rolesOfUser))
    case rolesOfApp: SetRolesForApp =>
      logger.trace("Set roles for app")
      sender() ! Changed
      context.become(lookingUp(caches + rolesOfApp))
  }

  def clearingCaches(caches: Caches): Receive = {
    case updateRoles@UpdateRoles(_, appServerDispatcher, userSession, requestSource) =>
      logger.trace("Update roles")
      sender() ! Changed
      val newCaches = caches + updateRoles
      getRolesFromSimplifier(newCaches) pipeTo self
      context.become(clearingCaches(newCaches))
    case SetPermissionsForRoles(rolesWithPermissions) =>
      logger.trace(s"Clearing cache: ${rolesWithPermissions.map(_.name).mkString(", ")}")
      sender() ! Changed
      val newRoleCache = caches.roleCache ++ rolesWithPermissions.map { roles =>
        roles.name -> roles.permissions
      }
      unstashAll()
      context.setReceiveTimeout(Duration.Inf)
      context.become(lookingUp(Caches(definedRoles = caches.definedRoles, roleCache = newRoleCache)))
    case ReceiveTimeout if caches.appServerSessionOpt.isDefined =>
      logger.error("Cache ran into timeout")
      getRolesFromSimplifier(caches) pipeTo self
      context.setReceiveTimeout(receiveTimeoutClearingCaches)
    case ReceiveTimeout =>
      logger.error("No token")
    case _ =>
      logger.trace("Stash lookups")
      stash()
  }

  def getRolesFromSimplifier(caches: Caches): Future[SetPermissionsForRoles] = {
    val appServerSession = caches.appServerSessionOpt.get
    implicit val userSession: UserSession = appServerSession.userSession
    implicit val requestSource: RequestSource = appServerSession.requestSource
    appServerSession.appServerDispatcher.getAllRolesWithPermission(caches.definedRoles.toSeq)
      .map(roles => SetPermissionsForRoles(roles.roleList))
  }

  override def postStop(): Unit = {
    logger.error("Role Cache died.")
    super.postStop()
  }

  case class AppServerSession(appServerDispatcher: AppServerDispatcher, userSession: UserSession, requestSource: RequestSource)

  case class Caches(roleCache: Map[String, Seq[MinimalPermissionInfo]] = Map(),
                    userCache: Map[Long, Set[String]] = Map(),
                    appCache: Map[String, Set[String]] = Map(),
                    definedRoles: Set[String] = Set(),
                    appServerSessionOpt: Option[AppServerSession] = None) {

    def +(updateRoles: UpdateRoles): Caches = {
      copy(
        definedRoles = definedRoles union updateRoles.roles,
        appServerSessionOpt = Some(AppServerSession(updateRoles.appServerDispatcher, updateRoles.userSession, updateRoles.requestSource))
      )
    }

    def +(userRoles: SetRolesForUser): Caches = {
      val newUserCache = userCache + (userRoles.userId -> userRoles.roles.toSet)
      copy(userCache = newUserCache)
    }

    def +(rolesForApp: SetRolesForApp): Caches = {
      val newAppCache = appCache + (rolesForApp.app -> rolesForApp.roles.toSet)
      copy(appCache = newAppCache)
    }

    def roles: CachedRoles = {
      CachedRoles(roleCache.keys.toSeq)
    }

    def rolesOfUser(userId: Long): Roles = {
      Roles(userCache.get(userId))
    }

    def rolesOfApp(app: String): Roles = {
      Roles(appCache.get(app))
    }

    def permissionsOfRoles(roles: Seq[String]): PermissionsForRoles = {
      val permissions = roles.foldLeft(Seq.empty[GrantedPermission]) {
        case (perms, role) =>
          perms ++ roleCache.getOrElse(role, Seq()).map(grantedPermission)
      }
      PermissionsForRoles(roles, permissions)
    }

    def grantedPermission(permission: MinimalPermissionInfo): GrantedPermission = {
      val characteristics = permission.characteristicsWithValues.map { charWithValues =>
        (charWithValues.characteristic, charWithValues.values)
      }.toMap
      GrantedPermission(permission.permissionName, characteristics)
    }

    def permissionsOfUser(userId: Long): PermissionsForUser = {
      val permissions = for {
        role <- userCache.getOrElse(userId, Seq())
        permission <- roleCache.getOrElse(role, Seq())
      } yield grantedPermission(permission)
      PermissionsForUser(userId, Some(permissions.toSeq))
    }

    def permissionsOfApp(app: String): PermissionsForApp = {
      val permissions = for {
        role <- appCache.getOrElse(app, Seq())
        permission <- roleCache.getOrElse(role, Seq())
      } yield grantedPermission(permission)
      PermissionsForApp(app, Some(permissions.toSeq))
    }

  }
}

object RoleCacheActor {

  val roleCachePromise: Promise[ActorRef] = Promise()

  private[helpers] val roleCacheActor: Future[ActorRef] = roleCachePromise.future

  def apply(system: ActorSystem): Future[ActorRef] = {
    if (!roleCachePromise.isCompleted) {
      roleCachePromise.success(system.actorOf(Props(new RoleCacheActor)))
    }
    roleCachePromise.future
  }

  case class UpdateRoles(roles: Set[String], appServerDispatcher: AppServerDispatcher, userSession: UserSession, requestSource: RequestSource)
  sealed trait UpdateState {
    def toBoolean: Boolean
  }
  case object Changed extends UpdateState {
    override val toBoolean: Boolean = true
  }
  case object Unchanged extends UpdateState {
    override val toBoolean: Boolean = false
  }
  case object LookUpCachedRoles
  case class CachedRoles(roles: Seq[String])
  case class LookUpPermissionsForRoles(role: Seq[String])
  case class PermissionsForRoles(roles: Seq[String], permissions: Seq[GrantedPermission])
  case class LookUpPermissionsForUser(userId: Long)
  case class PermissionsForUser(userId: Long, permissions: Option[Seq[GrantedPermission]])
  case class LookUpRolesForUser(userId: Long)
  case class LookUpRolesForApp(app: String)
  case class Roles(roleNames: Option[Set[String]])
  case class LookUpPermissionsForApp(app: String)
  case class PermissionsForApp(app: String, permissions: Option[Seq[GrantedPermission]])
  case class SetRolesForUser(userId: Long, roles: Seq[String])
  case class SetRolesForApp(app: String, roles: Seq[String])
  case class SetPermissionsForRoles(rolesWithPermissions: Seq[RoleWithPermissions])

}
