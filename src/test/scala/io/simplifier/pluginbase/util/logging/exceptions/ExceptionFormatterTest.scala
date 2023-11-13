package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionFormatterTest extends AnyWordSpec with Matchers {
  "An exception cause formatter" when {
    "returning the message" should {
      "always return the correct message when a message is available" in new CauseFixture {
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_CAUSE_1, None) mustBe EXCEPTION_WITH_CAUSE_1_MESSAGE
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_CAUSE_2, None) mustBe EXCEPTION_WITH_CAUSE_2_MESSAGE
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_CAUSE_3, None) mustBe EXCEPTION_WITH_CAUSE_3_MESSAGE
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_CAUSE_4, None) mustBe EXCEPTION_WITH_CAUSE_4_MESSAGE
      }

      "always return the default message when no message is available and no fallback message was not provided" in new CauseFixture {
        FORMATTER.getExceptionMessage(null, null) mustBe FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getExceptionMessage(null, None) mustBe FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_NO_CAUSE, None) mustBe EXCEPTION_WITH_NO_CAUSE_MESSAGE
        FORMATTER.getExceptionMessage(EMPTY_EXCEPTION_1, None) mustBe FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getExceptionMessage(EMPTY_EXCEPTION_2, None) mustBe FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getExceptionMessage(EMPTY_EXCEPTION_3, None) mustBe FALLBACK_MESSAGE_DEFAULT
      }

      "always return the fallback message when no message is available and a fallback message was provided" in new CauseFixture {
        FORMATTER.getExceptionMessage(null, Some(FALLBACK_MESSAGE)) mustBe FALLBACK_MESSAGE
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_NO_CAUSE, Some(FALLBACK_MESSAGE)) mustBe EXCEPTION_WITH_NO_CAUSE_MESSAGE
        FORMATTER.getExceptionMessage(EMPTY_EXCEPTION_1, Some(FALLBACK_MESSAGE)) mustBe FALLBACK_MESSAGE
        FORMATTER.getExceptionMessage(EMPTY_EXCEPTION_2, Some(FALLBACK_MESSAGE)) mustBe FALLBACK_MESSAGE
        FORMATTER.getExceptionMessage(EMPTY_EXCEPTION_3, Some(FALLBACK_MESSAGE)) mustBe FALLBACK_MESSAGE
      }

      "always return the default message when an error occurs and no fallback message was not provided" in new CauseFixture {
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_ERROR_1, None) mustBe FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_ERROR_2, None) mustBe FALLBACK_MESSAGE_DEFAULT
      }

      "always return the fallback message when an error occurs and a fallback message was provided" in new CauseFixture {
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_ERROR_1, Some(FALLBACK_MESSAGE)) mustBe FALLBACK_MESSAGE
        FORMATTER.getExceptionMessage(EXCEPTION_WITH_ERROR_2, Some(FALLBACK_MESSAGE)) mustBe FALLBACK_MESSAGE
      }
    }

    "returning the exception type" should {
      "always return the correct exception type" in new CauseFixture {
        FORMATTER.getExceptionType(null, null, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getExceptionType(null, ExceptionFormatterUtils.SIMPLE, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getExceptionType(null, ExceptionFormatterUtils.CANONICAL, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getExceptionType(null, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getExceptionType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_SIMPLE)
        FORMATTER.getExceptionType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_CANONICAL)
        FORMATTER.getExceptionType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EXCEPTION_WITH_NO_CAUSE, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EMPTY_EXCEPTION_1, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EMPTY_EXCEPTION_2, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EMPTY_EXCEPTION_3, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
      }

      "always return the correct exception type with a fallback when applicable" in new CauseFixture {
        FORMATTER.getExceptionType(null, ExceptionFormatterUtils.SIMPLE, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(FALLBACK_CLASS_NAME)
        FORMATTER.getExceptionType(null, ExceptionFormatterUtils.CANONICAL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(FALLBACK_CLASS_NAME)
        FORMATTER.getExceptionType(null, ExceptionFormatterUtils.FULL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(FALLBACK_CLASS_NAME)
        FORMATTER.getExceptionType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.SIMPLE, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_SIMPLE)
        FORMATTER.getExceptionType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.CANONICAL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_CANONICAL)
        FORMATTER.getExceptionType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.FULL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EXCEPTION_WITH_NO_CAUSE, ExceptionFormatterUtils.FULL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EMPTY_EXCEPTION_1, ExceptionFormatterUtils.FULL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EMPTY_EXCEPTION_2, ExceptionFormatterUtils.FULL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
        FORMATTER.getExceptionType(EMPTY_EXCEPTION_3, ExceptionFormatterUtils.FULL, Some(FALLBACK_CLASS_NAME), removeCompanionObjectSuffix = false) mustBe Some(EXCEPTION_TYPE_FULL)
      }
    }
  }

  trait BaseFixture {
    val FORMATTER: ExceptionFormatter = ExceptionFormatter.apply
    val FORMATTER2: ExceptionFormatter = ExceptionFormatter(new Names())
    val EMPTY_MESSAGE_1: String = null
    val DEEPEST_CAUSE_MESSAGE: String = "The bad boy is here"
    val EXCEPTION_TYPE_SIMPLE: String = "Exception"
    val EXCEPTION_TYPE_CANONICAL: String = "java.lang.Exception"
    val EXCEPTION_TYPE_FULL: String = "java.lang.Exception"

    val EXCEPTION: Throwable = new Exception("Base Exception")
    val EMPTY_EXCEPTION_1: Throwable = new Exception(EMPTY_MESSAGE_1)
    val EMPTY_EXCEPTION_2: Throwable = new Exception("")
    val EMPTY_EXCEPTION_3: Throwable = new Exception(" ")
    val DEEPEST_CAUSE: Throwable = new NullPointerException(DEEPEST_CAUSE_MESSAGE)
    val CAUSE_1: Throwable = new IllegalArgumentException("Cause 1", DEEPEST_CAUSE)
    val CAUSE_2: Throwable = new IllegalArgumentException("Cause 2", CAUSE_1)
    val CAUSE_3: Throwable = new RuntimeException("Cause_3", CAUSE_2)
    val CAUSE_4: Throwable = new RuntimeException("Cause_4", CAUSE_3)

    FORMATTER.setNames(new Names())
  }

  trait CauseFixture extends BaseFixture {
    val FALLBACK_MESSAGE_DEFAULT: String = "-- No Exception Message was available --"
    val FALLBACK_MESSAGE: String = "No message there but I am here"
    val FALLBACK_CLASS_NAME: String = "Hi, I am fallback class"

    class BadException extends Exception() {
      override def getCause: Throwable = throw new Exception("Not gonna cause!")
    }

    class BadException2 extends Exception() {
      override def getMessage: String = throw new Exception("Not gonna message!")
    }

    val EXCEPTION_WITH_CAUSE_1_MESSAGE: String = "Base Exception1"
    val EXCEPTION_WITH_CAUSE_2_MESSAGE: String = "Base Exception2"
    val EXCEPTION_WITH_CAUSE_3_MESSAGE: String = "Base Exception3"
    val EXCEPTION_WITH_CAUSE_4_MESSAGE: String = "Base Exception4"
    val EXCEPTION_WITH_NO_CAUSE_MESSAGE: String = "Base Exception"

    val EXCEPTION_WITH_ERROR_1: Throwable = new BadException()
    val EXCEPTION_WITH_ERROR_2: Throwable = new BadException2()
    val EXCEPTION_WITH_NO_CAUSE: Throwable = new Exception(EXCEPTION_WITH_NO_CAUSE_MESSAGE)
    val EXCEPTION_WITH_CAUSE_1: Throwable = new Exception(EXCEPTION_WITH_CAUSE_1_MESSAGE, CAUSE_1)
    val EXCEPTION_WITH_CAUSE_2: Throwable = new Exception(EXCEPTION_WITH_CAUSE_2_MESSAGE, CAUSE_2)
    val EXCEPTION_WITH_CAUSE_3: Throwable = new Exception(EXCEPTION_WITH_CAUSE_3_MESSAGE, CAUSE_3)
    val EXCEPTION_WITH_CAUSE_4: Throwable = new Exception(EXCEPTION_WITH_CAUSE_4_MESSAGE, CAUSE_4)
  }
}