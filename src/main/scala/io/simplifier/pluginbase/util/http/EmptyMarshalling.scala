package io.simplifier.pluginbase.util.http

import akka.http.scaladsl.marshalling.{Marshaller, ToEntityMarshaller}
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.unmarshalling.{FromEntityUnmarshaller, FromResponseUnmarshaller, Unmarshal, Unmarshaller}
import akka.http.scaladsl.util.FastFuture
import akka.stream.scaladsl.Sink

/**
  * Marshallers/Unmarshallers for empty messages.
  */
trait EmptyMarshalling {

  /**
    * Unmarshaller for unit, discarding the request entity to clear stream.
    */
  implicit val umUnit: FromEntityUnmarshaller[Unit] =
    Unmarshaller.withMaterializer(implicit ec => implicit mat => {
      case _: HttpEntity.Strict => FastFuture.successful(())
      case entity => entity.dataBytes.runWith(Sink.ignore) map { _ => }
    })

  /**
    * Unmarshaller for unit, discarding the request entity to clear stream.
    */
  implicit val responseUmEmpty: FromResponseUnmarshaller[Unit] =
    Unmarshaller.withMaterializer(implicit ec => implicit mat => {
      resp => Unmarshal(resp.entity).to[Unit]
    })

  /**
    * Marshaller for unit, creating an empty entity.
    */
  implicit val emUnit: ToEntityMarshaller[Unit] =
    Marshaller.opaque(_ => HttpEntity.Empty)

}

object EmptyMarshalling extends EmptyMarshalling