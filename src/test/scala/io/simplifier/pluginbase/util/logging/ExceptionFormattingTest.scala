package io.simplifier.pluginbase.util.logging

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import org.json4s.{JValue, _}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

/**
  * Delta tests, as each part was testes thoroughly in the respective classes.
  */
class ExceptionFormattingTest extends AnyWordSpec with Matchers {
  "The exception formatting methods" when {
    "getting the exception type" should {
      "always return the correct exception type" in new BaseFixture {
        getExceptionTypeSafely(null) mustBe EXCEPTION_TYPE_NOT_RETRIEVABLE
        getExceptionTypeSafely(null, ERROR_FALLBACK_TYPE) mustBe ERROR_FALLBACK_TYPE
      }
    }
    "getting the exception message" should {
      "always return the correct exception message" in new BaseFixture {
        getExceptionMessageSafely(EXCEPTION2) mustBe EXCEPTION_MESSAGE_NOT_AVAILABLE
        getExceptionMessageSafely(EXCEPTION2, ERROR_FALLBACK_MESSAGE) mustBe ERROR_FALLBACK_MESSAGE
      }
    }

    "getting the exception's root cause type with fallback" should {
      "always return the correct exception's root cause type or the fallback" in new BaseFixture {
        getCauseTypeSafelyWithFallback(null) mustBe EXCEPTION_AND_ROOT_CAUSE_TYPE_NOT_RETRIEVABLE
        getCauseTypeSafelyWithFallback(null, ERROR_FALLBACK_TYPE_FALLBACK) mustBe ERROR_FALLBACK_TYPE_FALLBACK
      }
    }
    "getting the exception's root cause message with fallback" should {
      "always return the correct exception's root cause message or the fallback" in new BaseFixture {
        getCauseMessageSafelyWithFallback(null) mustBe EXCEPTION_AND_ROOT_CAUSE_MESSAGE_NOT_RETRIEVABLE
        getCauseMessageSafelyWithFallback(null, ERROR_FALLBACK_MESSAGE_FALLBACK) mustBe ERROR_FALLBACK_MESSAGE_FALLBACK
      }
    }

    "getting the class name" should {
      "always return the correct class name" in new BaseFixture {
        getClassNameSafely(CAUSE) mustBe EXCEPTION_CANONICAL_AND_FULL_CLASSNAME
        getClassNameSafely(CAUSE, SimpleClassNames) mustBe EXCEPTION_SIMPLE_CLASSNAME
        getClassNameSafely(CAUSE, CanonicalClassNames) mustBe EXCEPTION_CANONICAL_AND_FULL_CLASSNAME
        getClassNameSafely(CAUSE, FullClassNames) mustBe EXCEPTION_CANONICAL_AND_FULL_CLASSNAME
      }
    }

  }

  trait BaseFixture extends ExceptionFormatting {

    val CAUSE: NullPointerException = new NullPointerException("test")
    val CAUSE2: NullPointerException = new NullPointerException(null)
    val EXCEPTION: Exception = new Exception("Exception", CAUSE)
    val EXCEPTION2: Exception = new Exception(null, CAUSE2)
    val EXCEPTION3: Exception = new Exception("Exception3", null)
    val ERROR_FALLBACK_TYPE: String = "Error Fallback for type here"
    val ERROR_FALLBACK_ROOT_CAUSE_TYPE: String = "Error Fallback for root type here"
    val ERROR_FALLBACK_TYPE_FALLBACK: String = "Error Fallback for type and root type here"
    val ERROR_FALLBACK_MESSAGE: String = "Error Fallback for message here"
    val ERROR_FALLBACK_ROOT_CAUSE_MESSAGE: String = "Error Fallback root for message here"
    val ERROR_FALLBACK_MESSAGE_FALLBACK: String = "Error Fallback for message and root message here"

    val EXCEPTION_TYPE: String = "java.lang.Exception"
    val EXCEPTION_SIMPLE_CLASSNAME: String = "NullPointerException"
    val EXCEPTION_CANONICAL_AND_FULL_CLASSNAME: String = "java.lang.NullPointerException"

    val EXCEPTION_TYPE_NOT_RETRIEVABLE: String = "-- The exception type was not retrievable --"
    val EXCEPTION_ROOT_CAUSE_TYPE_NOT_RETRIEVABLE: String = "-- The exception's root cause type was not retrievable --"
    val EXCEPTION_AND_ROOT_CAUSE_TYPE_NOT_RETRIEVABLE: String = "-- Neither the exception's root cause type nor the exception cause type was retrievable --"

    val EXCEPTION_MESSAGE_NOT_AVAILABLE: String = "-- The exception message was not available --"
    val EXCEPTION_ROOT_CAUSE_MESSAGE_NOT_AVAILABLE: String = "-- The exception's root cause message was not available --"
    val EXCEPTION_AND_ROOT_CAUSE_MESSAGE_NOT_RETRIEVABLE: String = "-- Neither the exception's root cause message nor the exception cause message was retrievable --"

    val EXCEPTION_DETAILS: JValue =
      JObject(
        JField("Type", JString(EXCEPTION_TYPE)),
        JField("Message", JString("Exception3")),
        JField("Details", JNothing),
        JField("Cause", JNothing),
        JField("Suppressed-Errors", JNothing)
      )

    val EXCEPTION_DETAILS2: JValue =
      JObject(
        JField("Type", JString(EXCEPTION_CANONICAL_AND_FULL_CLASSNAME)),
        JField("Message", JString("-")),
        JField("Details", JNothing),
        JField("Cause", JNothing),
        JField("Suppressed-Errors", JNothing)
      )

    val EXCEPTION_DETAIL3: JValue =
      JObject(
        JField("Type", JString("-- The object has not been provided --")),
        JField("Message", JString("-")),
        JField("Details", JNothing),
        JField("Cause", JNothing),
        JField("Suppressed-Errors", JNothing)
      )

    val NAMES: Names =
      Names(
        component = Some("comp"),
        subComponent = None,
        function = None,
        subFunction = None,
        client = None,
        clientOperation = None,
        connector = None,
        connectorCall = None
      )

    val nullString: String = null
    val npe: Try[String] = Try(nullString.toLowerCase)

    val chainedException = new Exception("Something went wrong", npe.failed.get)
  }

}
