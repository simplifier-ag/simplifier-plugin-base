package io.simplifier.pluginbase.interfaces

import akka.http.scaladsl.marshalling.ToResponseMarshaller
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import io.simplifier.pluginapi.UserSession
import io.simplifier.pluginapi.rest.PluginHeaders.RequestSource
import io.simplifier.pluginbase.PluginDescription
import io.simplifier.pluginbase.interfaces.DefaultSlotInterfaceService.SlotMagnet
import io.simplifier.pluginbase.permission.PluginPermissionObject
import io.simplifier.pluginbase.util.http.CommonDirectives.discardEntity
import io.simplifier.pluginbase.util.json.SimplifierFormats

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

/**
  * Default implementation for Slot interfaces.
  */
abstract class DefaultSlotInterfaceService(dispatcher: AppServerDispatcher, pluginDescription: PluginDescription,
                                           pluginPermission: PluginPermissionObject)
  extends SlotInterfaceService(dispatcher, pluginDescription, pluginPermission) with SimplifierFormats {

  private var slotBuilder: Map[String, InnerServiceRoute] = Map.empty

  type InnerServiceRoute = (UserSession, RequestSource) => Route

  /**
    * Define slot with execution.
    * @param slotName slot name
    * @param magnet slot magnet
    */
  def slot(slotName: String, magnet: SlotMagnet): Unit = {
    val route: InnerServiceRoute = { (userSession, requestSource) =>
      path(slotName) {
        magnet(userSession, requestSource)
      }
    }
    slotBuilder += slotName -> route
  }

  /**
    * Define multiple slots with same execution.
    * @param slots slot names
    * @param magnet slot magnet
    */
  def slot(slots: Seq[String], magnet: SlotMagnet): Unit = slots.foreach(slot(_, magnet))

  /**
    * Define pair of slots (with and without "Http" suffix) with execution.
    * This combinations were used by plugins implemented in the past with the Akka Slot interface.
    * @param slotName slot name
    * @param magnet slot magnet
    */
  def slotWithHttp(slotName: String, magnet: SlotMagnet): Unit = {
    slot(Seq(slotName, slotName + "Http"), magnet)
  }

  /* slot names and route lazily evaluated after initialization has taken place */

  lazy val pluginSlotNames: Seq[String] = slotBuilder.keySet.toSeq.sorted

  def slotRoute: InnerServiceRoute = {
    val allRoutes = slotBuilder.values.toList
    if (allRoutes.isEmpty) throw new IllegalStateException("No slots defined")
    val emptyRoute: InnerServiceRoute = (_, _) => reject
    val combinedRoute = (allRoutes foldLeft emptyRoute){
      (prevRoutes, nextRoute) =>
        // combine all sub-routes together
        (userSession, requestSource) =>
        prevRoutes(userSession, requestSource) ~ nextRoute(userSession, requestSource)
    }
    combinedRoute
  }

  private lazy val builtServiceRoute = slotRoute

  override def serviceRoute(implicit requestSource: RequestSource, userSession: UserSession): Route =
    builtServiceRoute(userSession, requestSource)

}

object DefaultSlotInterfaceService {


  /**
    * Slot magnet, providing polymorphism for slot execution methods.
    */
  sealed trait SlotMagnet {

    def apply(userSession: UserSession, requestSource: RequestSource): Route

  }

  object SlotMagnet {

    implicit def fromEmpty[B](func: => B)(implicit em: ToResponseMarshaller[B]): SlotMagnet = new SlotMagnet {

      def apply(userSession: UserSession, requestSource: RequestSource): Route =
        discardEntity {
          complete(func)
        }

    }

    implicit def fromEmptySession[B](func: UserSession => B)
                                    (implicit em: ToResponseMarshaller[B]): SlotMagnet = new SlotMagnet {

      def apply(userSession: UserSession, requestSource: RequestSource): Route =
        discardEntity {
          complete(func(userSession))
        }

    }

    implicit def fromFunc1[A, B](func: A => B)
                                (implicit m1: FromEntityUnmarshaller[A],
                                 m2: ToResponseMarshaller[B]): SlotMagnet = new SlotMagnet {

      def apply(userSession: UserSession, requestSource: RequestSource): Route =
        entity(as[A]) { param =>
          complete(func(param))
        }

    }

    implicit def fromFunc1Session[A, B](func: (A, UserSession) => B)
                                       (implicit m1: FromEntityUnmarshaller[A],
                                        m2: ToResponseMarshaller[B]): SlotMagnet = new SlotMagnet {

      def apply(userSession: UserSession, requestSource: RequestSource): Route =
        entity(as[A]) { param =>
          complete(func(param, userSession))
        }

    }

    implicit def fromFunc3[A, B](func: (A, UserSession, RequestSource) => B)
                                (implicit m1: FromEntityUnmarshaller[A],
                                 m2: ToResponseMarshaller[B]): SlotMagnet = new SlotMagnet {

      def apply(userSession: UserSession, requestSource: RequestSource): Route =
        entity(as[A]) { param =>
          complete(func(param, userSession, requestSource))
        }

    }

    implicit def fromFunc3ToAsync[A, B](func: (A, UserSession, RequestSource, ExecutionContext) => Future[B])
                                (implicit m1: FromEntityUnmarshaller[A],
                                 m2: ToResponseMarshaller[B]): SlotMagnet = new SlotMagnet {

      def apply(userSession: UserSession, requestSource: RequestSource): Route =
        entity(as[A]) { param =>
          extractExecutionContext { ec =>
            onComplete(func(param, userSession, requestSource, ec))(result => complete(result.get))
          }
        }

    }

  }

}


