package io.simplifier.pluginbase.util.json

import io.simplifier.pluginbase.util.json.JSONCompatibility.parseJsonOrEmptyString
import io.simplifier.pluginbase.util.json.NamedTupleAccess._
import io.simplifier.pluginbase.util.logging.ExceptionFormatting
import org.json4s.jackson.Serialization
import org.json4s.{Formats, _}

import scala.util.Try

/**
 * The JSON-Formatter provides utility functions for formatting/extracting certain parts of it.
 */
object JSONFormatter extends ExceptionFormatting {

  private val Formats: Formats = SimplifierFormats.formats

  final private val JSON_CLASS_FIELD: String = "jsonClass"


  /**
   * Decomposes a value into a JSON.
   *
   * @note the <b>jsonClass</b> reference field was formerly known as <b>$outer</b>.
   * @param value       the value to decompose.
   * @param filterOuter the flag, whether the <b>jsonClass</b> reference field should be filtered out.
   * @param formats     the implicit JSON-Formats.
   * @return the decomposed value as a JSON.
   */
  def decomposeJSON(value: Any,
                    filterOuter: Boolean = false)
                   (implicit formats: Formats = Formats): JValue = {
    val decomposedJSON: JValue = Extraction.decompose(value)
    if (filterOuter) removeTypeHints(decomposedJSON) else decomposedJSON
  }

  /**
   * Parse JSON from provided byte array
   *
   * @param jsonBytes byte string to parse or null
   * @return parsed JValue on success
   */
  def parseJSON(jsonBytes: Array[Byte]): Try[JValue] = Try {
    if (jsonBytes == null) JNull else parseJsonOrEmptyString(jsonBytes)
  }

  /**
   * Renders a JSON compactly with regard to [[JNothing]].
   *
   * @note # [[JNothing]] will be rendered as an empty string without quotes.
   *       # Should [[JNothing]] be still inside objects and other arrays it will omitted
   *       this is due to the implementation of the [[Serialization.writePretty]] method.
   * @param json    the JSON to render compactly.
   * @param formats the implicit JSON-Formats.
   * @return the compactly rendered JSON.
   */
  def renderJSONCompact(json: JValue)
                       (implicit formats: Formats = Formats): String =
    Serialization.write(json)

  /**
   * Renders a JSON pretty with regard to [[JNothing]].
   *
   * @note # [[JNothing]] will be rendered as an empty string without quotes.
   *       # Should [[JNothing]] be still inside objects and other arrays it will omitted
   *       this is due to the implementation of the [[Serialization.writePretty]] method.
   * @param json    the JSON to render pretty.
   * @param formats the implicit JSON-Formats.
   * @return the pretty rendered JSON.
   */
  def renderJSONPretty(json: JValue)
                      (implicit formats: Formats = Formats): String =
    Serialization.writePretty(json)


  /**
   * Removes 'jsonClass' fields from a JValue recursively
   *
   * @note the <b>jsonClass</b> reference field was formerly known as <b>$outer</b>.
   * @note the <b>jsonClass</b> reference field can be configured in the formats
   * @note the outer reference occurs when a type hint is necessary.
   * @param json input json
   * @return cleared json
   */
  private def removeTypeHints(json: JValue): JValue = {
    json match {
      case JObject(fields) => JObject(fields.collect { case JField(name, value) if name != JSON_CLASS_FIELD => JField(name, removeTypeHints(value)) })
      case JArray(values) => JArray(values.map(removeTypeHints))
      case _ => json.removeField(f => f.name == JSON_CLASS_FIELD)
    }
  }
}