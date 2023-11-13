package io.simplifier.pluginbase.util.json

import org.json4s.JValue

import scala.language.implicitConversions

/**
  * unlike in liftweb.json in json4s' JField implementation the attribute name can only be accessed with _1
  * and the value can only be accessed with _2
  *
  * This method is meant to allow also the named access with json4s
  */
object NamedTupleAccess {
  implicit def tupleToMapEntry(tuple: (String, JValue)): JFieldLike = {
    JFieldLike(tuple._1, tuple._2)
  }

  /**
    * @param name the json attribute name
    * @param value the json attribute value
    */
  case class JFieldLike(name: String, value: JValue)
}
