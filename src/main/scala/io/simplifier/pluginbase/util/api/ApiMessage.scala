package io.simplifier.pluginbase.util.api

import akka.http.scaladsl.marshalling.ToEntityMarshaller
import akka.http.scaladsl.unmarshalling.FromEntityUnmarshaller
import io.simplifier.pluginbase.util.http.JsonMarshalling
import io.simplifier.pluginbase.util.http.JsonMarshalling.{JsonRenderCompact, JsonRenderSetting, explicitJsonTypeMarshaller, explicitJsonTypeUnmarshaller}
import org.json4s._

/**
  * Base trait for all message Case Classes handled as response to / request from the client.
  * There are marshallers/unmarshallers defined, which only need to be in scope in order to use any case class
  * extending `ApiMessage` in Akka Http.
  *
  * @see [[JsonMarshalling]]
  */
trait ApiMessage

trait StreamApiMessage extends ApiMessage {
  val data: Iterator[Array[Byte]]
}

/**
  * Companion object to ApiMessage, containing the marshalling/unmarshalling, included automatically.
  */
object ApiMessage {

  /**
    * `A` => HTTP entity, implicit and thus with a type limitation
    *
    * @tparam A type to encode, bounded by `ApiMessage`
    * @return marshaller for any `A` value
    */
  implicit def jsonTypeMarshaller[A <: ApiMessage](implicit formats: Formats,
                                                   renderSetting: JsonRenderSetting = JsonRenderCompact): ToEntityMarshaller[A] =
    explicitJsonTypeMarshaller[A]

  /**
    * HTTP entity => `A`, implicit and thus with a type limitation
    *
    * @tparam A type to decode, bounded by `ApiMessage`
    * @return unmarshaller for `A`
    */
  implicit def jsonTypeUnmarshaller[A <: ApiMessage : Manifest](implicit formats: Formats): FromEntityUnmarshaller[A] =
    explicitJsonTypeUnmarshaller[A]

}

/**
  * Simple message response.
  *
  * @param message Success message
  */
case class SuccessMessage(message: String) extends ApiMessage