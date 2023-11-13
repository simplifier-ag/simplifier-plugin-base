package io.simplifier.pluginbase.util.json

import org.json4s.jackson.JsonMethods.parse
import org.json4s.{JNothing, JValue}

import java.nio.charset.StandardCharsets


trait JSONCompatibility {
  /**
   * wrapper for parse() function, emulating liftweb behavior: "" parses to JNothing
   */
  @inline def parseJsonOrEmptyString(s: String): JValue = if (s.isEmpty) JNothing else parse(s)
  @inline def parseJsonOrEmptyString(b: Array[Byte]): JValue = parseJsonOrEmptyString(new String(b, StandardCharsets.UTF_8))

}

object JSONCompatibility extends JSONCompatibility

