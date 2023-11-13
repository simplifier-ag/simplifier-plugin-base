package io.simplifier.pluginbase.helpers

import akka.http.scaladsl.client.RequestBuilding.Post
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginApiMessage
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginbase.PluginSettings
import io.simplifier.pluginbase.interfaces.AppServerDispatcher
import io.simplifier.pluginbase.slotservice.Constants._
import io.simplifier.pluginbase.slotservice.GenericFailureHandling.OperationFailureMessage
import io.simplifier.pluginbase.util.json.JSONCompatibility.{parseJsonOrEmptyString => parseOk}
import org.json4s._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.util.{Failure, Success, Try}

abstract class PluginCommunication(appServerDispatcher: AppServerDispatcher,
                          pluginSettings: PluginSettings) {

  implicit val formats: DefaultFormats = DefaultFormats

  val pluginName: String

  /**
    * Calls a plugin slot and returns the response
    *
    * @param slotName the slot name
    * @param request the request
    * @param resultProperty the property where the result is stored
    * @param requestSource the implicit request source
    * @param userSession the implicit user session of the currently logged in user
    * @tparam A the request type
    * @tparam B the result type
    * @return the response of the slot
    */
  def callPlugin[A : Manifest : TypeTag, B <: PluginApiMessage]
  (slotName: String, request: B = null, resultProperty: Option[String] = Some("result"))
  (implicit requestSource: RequestSource, userSession: UserSession): A = {
    val url = s"/slot/$pluginName/$slotName"
    val req = if(request != null) {
      Post(url, request)
    } else {
      Post(url)
    }
    val future = appServerDispatcher.sendViaSimplifierFlow(req) map { response =>
      val responseJson = parseOk(AppServerDispatcher.renderEntity(response))
      if(typeOf[A] =:= typeOf[Unit] || typeOf[A] =:= typeOf[Nothing]) {
        Unit.asInstanceOf[A]
      } else {
        val response = resultProperty match {
          case None => responseJson
          case Some(property) => responseJson \ property
        }
        val errorFunction = (ex: Throwable) => {
          throw OperationFailureMessage(
            s"""Encountered error while calling slot '$slotName' of plugin '$pluginName'.
               |Error: '${ex.getMessage}.
               |Response: '${response}'.""".stripMargin,
            ERROR_CODE_CALL_PLUGIN)
        }
        Try(response.extract[A]) match {
          case Success(s) =>  s match {
            case resp: A => resp
            case ex: Throwable => errorFunction(ex)
          }
          case Failure(ex) => errorFunction(ex)
        }
      }

    }
    Await.result(future, pluginSettings.timeoutDuration)
  }
}

object PluginCommunication
