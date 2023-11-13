package io.simplifier.pluginbase.util.json

import io.simplifier.pluginbase.util.api.SuccessMessage
import org.json4s.Extraction.decompose
import org.json4s._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NullableOptionTest extends AnyWordSpec with Matchers {

  "NUndefined" should {
    "not be empty" in {
      NUndefined.isEmpty shouldBe false
    }
    "not be full" in {
      NUndefined.isFull shouldBe false
    }
    "be undefined" in {
      NUndefined.isUndefined shouldBe true
    }
    "throw an error on get" in {
      an [UnsupportedOperationException] shouldBe thrownBy {
        NUndefined.get
      }
    }
    "be deserialized from JNothing" in new JsonFixture {
      JNothing.extract[NullableOption[String]] shouldBe NUndefined
    }
    "be serialized to JNothing" in new JsonFixture {
      decompose(NUndefined) shouldBe JNothing
    }
  }

  "NEmpty" should {
    "be empty" in {
      NEmpty.isEmpty shouldBe true
    }
    "not be full" in {
      NEmpty.isFull shouldBe false
    }
    "not be undefined" in {
      NEmpty.isUndefined shouldBe false
    }
    "throw an error on get" in {
      an [UnsupportedOperationException] shouldBe thrownBy {
        NEmpty.get
      }
    }
    "be deserialized from JNull" in new JsonFixture {
      JNull.extract[NullableOption[String]] shouldBe NEmpty
    }
    "be serialized to JNull" in new JsonFixture {
      decompose(NEmpty) shouldBe JNull
    }
  }

  "NFull" should {
    "not be empty" in {
      NFull("abc").isEmpty shouldBe false
    }
    "be full" in {
      NFull("abc").isFull shouldBe true
    }
    "not be undefined" in {
      NFull("abc").isUndefined shouldBe false
    }
    "retrieve the value on get" in {
      NFull("abc").get shouldBe "abc"
      NFull(123).get shouldBe 123
      NFull(List(1,2, 3)).get shouldBe List(1,2, 3)
    }
    "be deserialized from JValue" in new JsonFixture {
      JString("abc").extract[NullableOption[String]] shouldBe NFull("abc")
      JInt(123).extract[NullableOption[Int]] shouldBe NFull(123)
      JBool(true).extract[NullableOption[Boolean]] shouldBe NFull(true)
      JObject(JField("message", JString("foo"))).extract[NullableOption[SuccessMessage]] shouldBe NFull(SuccessMessage("foo"))
      a[MappingException] shouldBe thrownBy {
        JBool(true).extract[NullableOption[SuccessMessage]]
      }
    }
    "be serialized to JValue" in new JsonFixture {
      decompose(NFull("abc")) shouldBe JString("abc")
      decompose(NFull(123)) shouldBe JInt(123)
      decompose(NFull(true)) shouldBe JBool(true)
      decompose(NFull(SuccessMessage("foo"))) shouldBe JObject(JField("message", JString("foo")))
    }
  }

  trait JsonFixture {
    implicit val formats: Formats = DefaultFormats + NullableOption.serializer
  }

}
