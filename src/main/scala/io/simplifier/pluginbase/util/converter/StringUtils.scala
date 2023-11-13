package io.simplifier.pluginbase.util.converter

object StringUtils {

  def removeQuotes(string: String,
                   removeBackSlashes: Boolean = false): String = {
    if(removeBackSlashes) {
      string.replace("\\\"", "")
    } else {
      string.replace("\"", "")
    }
  }

  def removeArrayBracketsAndQuotes(string: String): String = {
    Option(string).fold("")(_
      .replace("[", "")
      .replace("]", "")
      .replace("(", "")
      .replace(")", "")
      .replace("\"", "")
      .replace("\'", "")
      .trim)
  }

  def extractByteString(byteString: String): String = {
    Option(byteString).fold("")(bs => removeArrayBracketsAndQuotes(bs.toLowerCase).replace("bytestring", "").trim)
  }

  /**
   * Turn a string of format "FooBar" into snake case "foo_bar"
   *
   * Note: snakify is not reversible, ie. in general the following will _not_ be true:
   *
   * s == camelify(snakify(s))
   *
   * @return the underscored string
   */
  def snakify(name: String): String = name
    .replaceAll("([A-Z]+)([A-Z][a-z])", "$1_$2")
    .replaceAll("([a-z\\d])([A-Z])", "$1_$2")
    .toLowerCase

  /**
   * Turns a string of format "foo_bar" into camel case "FooBar"
   *
   * Functional code courtesy of Jamie Webb (j@jmawebb.cjb.net) 2006/11/28
   *
   * @note taken from net.liftweb + merged with the method camelifyMethod :)
   * @param name the String to CamelCase
    *
   * @return the CamelCased string
   */
  def camelify(name: String): String = {
    def loop(x: List[Char]): List[Char] = (x: @unchecked) match {
      case '_' :: '_' :: rest => loop('_' :: rest)
      case '_' :: c :: rest => Character.toUpperCase(c) :: loop(rest)
      case '_' :: Nil => Nil
      case c :: rest => c :: loop(rest)
      case Nil => Nil
    }

    name match {
      case null => ""
      case n if n.isEmpty => ""
      case n => loop('_' :: n.toList).mkString match {
        case cr if cr.length < 2 => cr.toLowerCase
        case cr => cr.substring(0, 1).toLowerCase + cr.substring(1)
      }
    }
  }

}