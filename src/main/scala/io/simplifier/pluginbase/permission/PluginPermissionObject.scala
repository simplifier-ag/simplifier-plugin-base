package io.simplifier.pluginbase.permission

import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginapi._
import io.simplifier.pluginbase.PluginDescription
import io.simplifier.pluginbase.helpers.RoleCaches
import io.simplifier.pluginbase.interfaces.AppServerDispatcher
import io.simplifier.pluginbase.permission.PluginPermissionObject.characteristicManagePermissions
import io.simplifier.pluginbase.permission.PluginPermissionObjectCharacteristics._
import io.simplifier.pluginbase.util.api.ApiMessage
import io.simplifier.pluginbase.util.json.JSONCompatibility.{parseJsonOrEmptyString => parseOk}
import io.simplifier.pluginbase.util.logging.Logging
import org.json4s._

import java.nio.file.{FileSystems, Files, Path, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Abstract class describing a plugin permission.
  */
abstract class PluginPermissionObject {

  /** Permission object display name */
  val name: String

  /** (Unique) permission object technical name */
  val technicalName: String

  /** description / tooltip */
  val description: String

  /** characteristics of the permission object, must not be empty */
  val characteristics: Seq[CharacteristicDefinition]

  def commonCharacteristics: Seq[CharacteristicDefinition] = Seq(
    CheckboxCharacteristic ( characteristicManagePermissions, "Manage permissions",
    "User is granted to download and upload a permission file", defaultValue = false)
  )

}

case class PluginPermissionFile(checkboxCharacteristics: Seq[CheckboxCharacteristic],
                                textfieldCharacteristics: Seq[TextFieldCharacteristic],
                                dropDownCharacteristics: Seq[DropDownCharacteristic],
                                multiSelectCharacteristics: Seq[MultiSelectCharacteristic]
                               ) extends ApiMessage

object PluginPermissionObject extends Logging {

  val characteristicManagePermissions = "managePermission"

  def getTechnicalName(pluginName: String): String = s"com.itizzimo.plugin.$pluginName"

  def apply(pluginDescription: PluginDescription): PluginPermissionObject = {
    apply(s"Plugin: ${pluginDescription.name}",
      PluginPermissionObject.getTechnicalName(pluginDescription.name),
      s"Plugin: Permissions for plugin ${pluginDescription.name}",
      Seq())
  }

  def apply(pName: String, pTechnicalName: String, pDescription: String, pCharacteristics: Seq[CharacteristicDefinition]): PluginPermissionObject = {
    new PluginPermissionObject {
      /** Permission object display name */
      override val name: String = pName
      /** (Unique) permission object technical name */
      override val technicalName: String = pTechnicalName
      /** description / tooltip */
      override val description: String = pDescription
      /** characteristics of the permission object, must not be empty */
      override val characteristics: Seq[CharacteristicDefinition] = pCharacteristics
    }
  }

  /**
    * Convert typed description of plugin permission object to the api message being transmitted.
    * @param permissionObject permission object to convert
    * @return api message
    */
  implicit def toApi(permissionObject: PluginPermissionObject): PermissionObjectDefinition =
    PermissionObjectDefinition(
      name = permissionObject.name,
      technicalName = permissionObject.technicalName,
      description = permissionObject.description,
      possibleCharacteristics = (permissionObject.characteristics ++ permissionObject.commonCharacteristics) .map {
        case CheckboxCharacteristic(technicalName, name, description, defaultValue) =>
          technicalName -> PermissionObjectCharacteristicDefinition(
            name = name,
            description = description,
            possibleValues = Some(Seq(true, false).map(_.toString)),
            default = defaultValue.toString,
            displayType = "checkbox"
          )
        case DropDownCharacteristic(technicalName, name, description, values, defaultValue) =>
          technicalName -> PermissionObjectCharacteristicDefinition(
            name = name,
            description = description,
            possibleValues = Some(values),
            default = defaultValue,
            displayType = "dropdown"
          )
        case TextFieldCharacteristic(technicalName, name, description, defaultValue) =>
          technicalName -> PermissionObjectCharacteristicDefinition(
            name = name,
            description = description,
            possibleValues = None,
            default = defaultValue,
            displayType = "textfield"
          )
        case MultiSelectCharacteristic(technicalName, name, description, values, defaultValue) =>
          technicalName -> PermissionObjectCharacteristicDefinition(
            name = name,
            description = description,
            possibleValues = Some(values.toSeq),
            default = defaultValue,
            displayType = "multiselect"
          )
      }.toMap
    )

  def getPermissionFilePath(pluginName: String): String = {
    s"/opt/simplifier/data/plugins/${pluginName}_permissions.json"
  }

  def getPermissionFile(pluginName: String): Option[Path] = {
    val path = this.getPermissionFilePath(pluginName)
    if(Files.exists(Paths.get(path)))
      Some(FileSystems.getDefault.getPath(path))
    else
      None
  }

  def readPermissionFile(pluginName: String): Option[PluginPermissionObject] = {

    import scala.collection.JavaConverters._
    implicit val formats: Formats = DefaultFormats

    Try {
      getPermissionFile(pluginName) match {
        case Some(pathToPermissionFile) =>
          (Try(Files.readAllLines(pathToPermissionFile)) match {
            case Success(v) => Some(v.asScala)
            case Failure(e) =>
              logger.error(s"Error during processing plugin permission file: ${pathToPermissionFile.toAbsolutePath}", e)
              None
          }) map { fileContent =>
            val fileAsJData = parseOk(fileContent.mkString)
            val permissionFile = fileAsJData.extract[PluginPermissionFile]
            val allCharacteristics = permissionFile.checkboxCharacteristics ++ permissionFile.dropDownCharacteristics ++
              permissionFile.textfieldCharacteristics ++ permissionFile.multiSelectCharacteristics
            Some(PluginPermissionObject(
              s"Plugin: $pluginName Custom",
              s"com.itizzimo.plugin.$pluginName.custom",
              s"Custom description for plugin $pluginName",
              allCharacteristics
            ))
          }
        case None =>
          logger.info(s"Permission file for plugin '$pluginName' could not be found.")
          None
      }
    } match {
      case Success(v) => v.flatten
      case Failure(e) =>
        logger.error("Error while processing permission file.", e)
        None
    }
  }

  /**
    * Loads all permissions for the user or app either from the cache or from the appserver
    *
    * @param permissionName the technical name of the permission
    * @param dispatcher the appserver dispatcher to be used
    * @param userSession the user session of the logged in user
    * @param requestSource the request source
    * @return all granted permissions for the user/app and permission
    */
  def loadPermissions(permissionName: String, dispatcher: AppServerDispatcher)
                     (implicit userSession: UserSession, requestSource: RequestSource): Future[GrantedPermissions] = {
    ((userSession.userIdOpt, userSession.appNameOpt) match {
      case (Some(userId), _) => RoleCaches.permissionsForUser(userId)
      case (_, Some(appName)) => RoleCaches.permissionsForApp(appName)
      case _ => Future.successful(Some(Seq.empty[GrantedPermission]))
    }).flatMap {
      case Some(permissions) => Future.successful(GrantedPermissions(permissions))
      case None => dispatcher.getAllRoleNames.flatMap(mapPermissions)
    }
  }

  private def mapPermissions(userRoles: Set[String]): Future[GrantedPermissions] = {
    RoleCaches.permissionsForRoles(userRoles.toSeq).map(GrantedPermissions)
  }

  def hasCharacteristic(technicalName: String, characteristic: String, expectedValue: String, permissions: GrantedPermissions): Boolean = {
    permissions.permissionObjects.find(_.technicalName == technicalName).map(_.characteristics)
      .exists(c => c.getOrElse(characteristic, Set()).contains(expectedValue))
  }

}