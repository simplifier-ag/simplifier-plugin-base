package io.simplifier.pluginbase.util.http

import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives.{as, entity, pass}
import EmptyMarshalling.umUnit

trait CommonDirectives {

  /**
    * Directive to discard a non-required request entity.
    */
  val discardEntity: Directive0 = entity(as[Unit]) flatMap { _ => pass }

}

object CommonDirectives extends CommonDirectives