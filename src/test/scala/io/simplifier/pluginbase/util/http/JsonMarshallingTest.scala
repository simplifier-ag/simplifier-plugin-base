package io.simplifier.pluginbase.util.http

import akka.http.scaladsl.model.StatusCodes.{OK, UnsupportedMediaType}
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.simplifier.pluginbase.util.json.SimplifierFormats
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
  * Test for JSON Marshalling.
  */
class JsonMarshallingTest extends AnyWordSpec with ScalatestRouteTest with Matchers with SimplifierFormats {

  import JsonMarshallingTest._

  implicit val um = JsonMarshalling.explicitJsonTypeUnmarshaller[TestEntity]
  implicit val em = JsonMarshalling.explicitJsonTypeMarshaller[TestEntity]

  val chuckNorris = "Chuck Norris"
  val chuckNorrisJson = """{"name":"Chuck Norris"}"""

  val route =
    Route.seal {
      post {
        entity(as[TestEntity]) { entity =>
          complete(entity.name)
        }
      } ~ get {
        complete {
          TestEntity(chuckNorris)
        }
      }
    }

  "A JSON unmarshaller" should {

    "Accept entities with content type application/json and UTF-8" in {
      val entity = HttpEntity(ContentTypes.`application/json`, chuckNorrisJson)
      val req = Post("/", entity)
      req ~> route ~> check {
        status mustBe OK
        entityAs[String] mustBe chuckNorris
      }
    }

    "Accept entities with content type application/json without charset" in {
      val entity = HttpEntity(ContentType(MediaType.custom("application/json", binary = true).asInstanceOf[MediaType.Binary]), chuckNorrisJson.getBytes)
      val req = Post("/", entity)
      req ~> route ~> check {
        status mustBe OK
        entityAs[String] mustBe chuckNorris
      }
    }

    "Accept entities without content type" in {
      val entity = HttpEntity(ContentTypes.NoContentType, chuckNorrisJson.getBytes)
      val req = Post("/", entity)
      req ~> route ~> check {
        status mustBe OK
        entityAs[String] mustBe chuckNorris
      }
    }

    "Deny entities with content type application/x-www-form-urlencoded for reasons" in {
      val entity = HttpEntity(ContentType(MediaTypes.`application/x-www-form-urlencoded`, () => HttpCharsets.`UTF-8`), chuckNorrisJson.getBytes)
      val req = Post("/", entity)
      req ~> route ~> check {
        status mustBe UnsupportedMediaType
      }
    }

    "Reject entities with different content type" in {
      val entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, chuckNorrisJson)
      val req = Post("/", entity)
      req ~> route ~> check {
        status mustBe UnsupportedMediaType
      }
    }
  }

  val routeMarshal = get {
    complete {
      TestEntity(chuckNorris)
    }
  }

  "A JSON marshaller" should {
    "produce content type application/json and encode the case class as JSON" in {
      val req = Get("/")
      req ~> route ~> check {
        status mustBe OK
        contentType mustBe ContentTypes.`application/json`
        entityAs[String] mustBe chuckNorrisJson
      }
    }
  }

}

object JsonMarshallingTest {

  case class TestEntity(name: String)

}