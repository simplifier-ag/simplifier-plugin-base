package io.simplifier.pluginbase.util.io

import akka.stream.Materializer
import akka.stream.scaladsl.Source
import akka.util.ByteString

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}

/**
  * Utilities for constructing streams.
  */
object StreamUtils {

  type ByteSource = Source[ByteString, Any]

  /**
    * Fold source to byte array.
    *
    * @param source source of ByteString
    * @return future of materialized byte array
    */
  def foldToByteArray(source: ByteSource)(implicit ec: ExecutionContext, materializer: Materializer): Future[Array[Byte]] = {
    source.runFold(ByteString.empty)(_ ++ _).map(_.toArray[Byte])
  }

}
