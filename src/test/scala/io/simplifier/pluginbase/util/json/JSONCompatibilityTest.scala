package io.simplifier.pluginbase.util.json

import org.json4s.{JField, JInt, JNothing, JObject}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JSONCompatibilityTest extends AnyWordSpec with Matchers {

  "JSONCompatibility" when {

    "parseJsonOrEmptyString" should {

      "parse non empty string as json value" in {
        val expected = JObject(JField("A", JInt(BigInt(123))) :: Nil)
        JSONCompatibility.parseJsonOrEmptyString("""{ "A": 123 }""") should be (expected)
      }

      "parse empty string as JNothing" in {
        JSONCompatibility.parseJsonOrEmptyString("") should be (JNothing)
      }
      "parse empty byte array as JNothing" in {
        JSONCompatibility.parseJsonOrEmptyString(Array.emptyByteArray) should be (JNothing)
      }
    }

  }

}