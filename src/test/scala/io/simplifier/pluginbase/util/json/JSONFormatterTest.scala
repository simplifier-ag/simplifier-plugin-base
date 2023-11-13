package io.simplifier.pluginbase.util.json

import io.simplifier.pluginbase.util.json.JSONFormatter._
import org.json4s._
import org.json4s.jackson.Serialization
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.charset.StandardCharsets
import scala.util.Success

class JSONFormatterTest extends AnyWordSpec with Matchers {


  "The JSON Formatter" when {
    "decomposing a JSON" must {
      "return the correct without the jsonClass reference, when filtered out" in new Decomposing {
        decomposeJSON(instanceOfOuterHelp, filterOuter = true)(formats) mustBe decomposedJsonWithoutOuter
      }
      "return the correct with the jsonClass reference, when filtered out" in new Decomposing {
        decomposeJSON(instanceOfOuterHelp, filterOuter = false)(formats) mustBe decomposedJsonWithJsonClass
      }
    }



    "parsing into JSON" must {
      "return the correct JSON for null or an empty string" in new Parsing {
        parseJSON(null: Array[Byte]) mustBe Success(JNull)
      }

      "return the correct JSON for a valid byte array without filtered jsonClass references" in new Parsing {
        parseJSON(byteWithJsonClass) mustBe Success(decomposedJsonWithJsonClass)
      }

      "return the correct JSON for null or an empty byte array" in new Parsing {
        parseJSON(null: Array[Byte]) mustBe Success(JNull)
        parseJSON(emptyByteArray) mustBe Success(JNothing)
      }

    }


    "rendering a JSON compactly" must {
      "return an empty string when JNothing is rendered" in new RenderingWithQuotes {
        renderJSONCompact(JNothing) mustBe ""
      }

      "return \"null\" when null is provided" in new RenderingWithQuotes {
        renderJSONCompact(null) mustBe "null"
      }

      "return the compact string when any other JSON is provided" in new RenderingWithQuotes {
        renderJSONCompact(complicatedJSON) mustBe complicatedJSONCompact
      }
    }

    "rendering a JSON pretty" must {
      "return an empty string when JNothing is rendered" in new RenderingWithQuotes {
        renderJSONPretty(JNothing) mustBe ""
      }

      "return \"null\" when null is provided" in new RenderingWithQuotes {
        renderJSONPretty(null) mustBe "null"
      }

      "return the compact string when any other JSON is provided" in new RenderingWithQuotes {
        renderJSONPretty(complicatedJSON) mustBe complicatedJSONPretty
      }
    }


  }


  trait BaseFixture {

    trait Animal

    case class Dog(name: String) extends Animal

    case class Fish(weight: Double) extends Animal


    case class OuterHelp(jsonClasss: List[Animal])



    val shortTypeHints: ShortTypeHints = ShortTypeHints(List(classOf[Dog], classOf[Fish]))
    val shortTypeHints2: ShortTypeHints = ShortTypeHints(List(classOf[Dog], classOf[Fish]),"jsonClass2")
    val shortTypeHints3: ShortTypeHints = ShortTypeHints(List(classOf[Dog], classOf[Fish]), null)
    val formats: Formats = Serialization.formats(shortTypeHints)
    val otherFormats: Formats = Serialization.formats(shortTypeHints2)
    val otherFormats2: Formats = Serialization.formats(shortTypeHints3)


  }


  trait Decomposing extends BaseFixture {
    val dogName: String = "doug"
    val fishWeight: Double = 11.1d
    val instanceOfOuterHelp: OuterHelp = OuterHelp(List(Fish(fishWeight), Dog(dogName)))

    val decomposedJsonWithJsonClass: JValue = JObject(
      JField("jsonClasss", JArray(
        List(
          JObject(
            JField("jsonClass", JString("JSONFormatterTest$BaseFixture$Fish")),
            JField("weight", JDouble(fishWeight))
          ),
          JObject(
            JField("jsonClass", JString("JSONFormatterTest$BaseFixture$Dog")),
            JField("name", JString(dogName))
          )
        )
      ))
    )


    val decomposedJsonWithoutOuter: JValue = JObject(
      JField("jsonClasss", JArray(
        List(
          JObject(JField("weight", JDouble(fishWeight))),
          JObject(List(("name", JString(dogName))))
        )
      ))
    )

  }


  trait FilterOuter extends Decomposing {
    val withoutJsonClass: String = "I am a string jsonClass, and jsonClass"
    val stringWithFilteredJsonClass = "{\"jsonClasss\":[{\"weight\":11.1},{\"name\":\"doug\"}]}"
    val stringWithJsonClass = "{\"jsonClasss\":[{\"jsonClass\":\"JSONFormatterTest$BaseFixture$Fish\",\"weight\":11.1},{\"jsonClass\":\"JSONFormatterTest$BaseFixture$Dog\",\"name\":\"doug\"}]}"
    val stringWithJsonClass2 = "{\"jsonClasss\":[{\"jsonClass2\":\"JSONFormatterTest$BaseFixture$Fish\",\"weight\":11.1},{\"jsonClass2\":\"JSONFormatterTest$BaseFixture$Dog\",\"name\":\"doug\"}]}"
    val stringWithJsonClass3 = "{\"jsonClasss\":[{\"null\":\"JSONFormatterTest$BaseFixture$Fish\",\"weight\":11.1},{\"null\":\"JSONFormatterTest$BaseFixture$Dog\",\"name\":\"doug\"}]}"

  }


  trait FormatKeys extends BaseFixture {
    def formatter(key: String): String = s"sexy-$key"

    val jField1: JField = JField("Field1", JNothing)
    val jField2: JField = JField("Field2", JNull)
    val jField3: JField = JField("Field3", JObject(
      jField1,
      jField2
    ))

    val jsonObject: JObject = JObject(
      jField1,
      jField2,
      jField3
    )


    val jFieldFormatted1: JField = JField("sexy-Field1", JNothing)
    val jFieldFormatted2: JField = JField("sexy-Field2", JNull)
    val jFieldFormatted3: JField = JField("sexy-Field3", JObject(
      jFieldFormatted1,
      jFieldFormatted2
    ))

    val jsonObjectFormatted: JObject = JObject(
      jFieldFormatted1,
      jFieldFormatted2,
      jFieldFormatted3
    )
  }


  trait GetJFields extends BaseFixture {
    val jField1: JField = JField("Field1", JNothing)
    val jField2: JField = JField("Field2", JNull)

    val jsonObject: JObject = JObject(jField1, jField2)

    val jFieldList: Seq[JField] = List(jField1, jField2)
  }


  trait Parsing extends FilterOuter {
    val emptyByteArray: Array[Byte] = Array.empty[Byte]
    val byteWithJsonClass: Array[Byte] = stringWithJsonClass.getBytes(StandardCharsets.UTF_8)
    val badJson: String = "Bad Apple"
    val badJsonArray: Array[Byte] = badJson.getBytes(StandardCharsets.UTF_8)
  }


  trait Rendering extends Parsing {

    val badFormats: Formats = DefaultFormats.lossless


    val prettyFiltered: String =
      """{
        |  "jsonClasss" : [ {
        |    "weight" : 11.1
        |  }, {
        |    "name" : "doug"
        |  } ]
        |}""".stripMargin

    val prettyUnfiltered: String =
      """{
        |  "jsonClasss" : [ {
        |    "jsonClass" : "JSONFormatterTest$BaseFixture$Fish",
        |    "weight" : 11.1
        |  }, {
        |    "jsonClass" : "JSONFormatterTest$BaseFixture$Dog",
        |    "name" : "doug"
        |  } ]
        |}""".stripMargin

    val fallbackJson: JArray = JArray(List(JInt(42), JInt(420)))

    val fallbackCompact: String = "[42,420]"
    val fallbackPretty: String =
      """[ 42, 420 ]""".stripMargin

  }


  trait RenderingWithQuotes extends BaseFixture {
    val quotedString: String = "\"Hello Fellow Children\""
    val quotedString2: String = "I am quoting \"my quotes, that quote \"my ot\"her \\ quotes\"\""

    val unquotedString: String = "\\Hello Fellow Children\\"
    val unquotedString2: String = "I am quoting \\my quotes, that quote \\my ot\\her \\\\ quotes\\\\"

    val unquotedStringWithoutBackslashes: String = "Hello Fellow Children"
    val unquotedString2WithoutBackslashes: String = "I am quoting my quotes, that quote my other \\\\ quotes"


    val quotedJsonString: JString = JString(quotedString)
    val quotedJsonString2: JString = JString(quotedString2)


    val complicatedJSON: JValue = {
      JArray(
        List(
          JNothing,
          JNull,
          JInt(42),
          JString("1337"),
          JDouble(23.21),
          JObject(
            JField("field", JNull)
          ),
          JArray(
            List(
              JInt(1),
              JNothing
            )
          )
        )
      )
    }

    val complicatedJSONCompact: String = "[null,42,\"1337\",23.21,{\"field\":null},[1]]"
    val complicatedJSONPretty: String =
      """[ null, 42, "1337", 23.21, {
        |  "field" : null
        |}, [ 1 ] ]""".stripMargin

  }

}
