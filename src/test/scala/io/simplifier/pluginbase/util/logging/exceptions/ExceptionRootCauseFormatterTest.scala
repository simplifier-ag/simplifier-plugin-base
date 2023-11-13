package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionRootCauseFormatterTest extends AnyWordSpec with Matchers {
  "An exception root cause formatter" when {
    "returning the root cause" should {
      "always return the correct and deepest root cause" in new CauseFixture {
        FORMATTER.getRootCause(null) mustBe None
        FORMATTER.getRootCause(EXCEPTION_WITH_NO_CAUSE) mustBe None
        FORMATTER.getRootCause(EXCEPTION_WITH_CAUSE_1) mustBe Some(DEEPEST_CAUSE)
        FORMATTER.getRootCause(EXCEPTION_WITH_CAUSE_2) mustBe Some(DEEPEST_CAUSE)
        FORMATTER.getRootCause(EXCEPTION_WITH_CAUSE_3) mustBe Some(DEEPEST_CAUSE)
        FORMATTER.getRootCause(EXCEPTION_WITH_CAUSE_4) mustBe Some(DEEPEST_CAUSE)
      }
    }

    "always return None when an error occurs" in new CauseFixture {
      FORMATTER.getRootCause(EXCEPTION_WITH_ERROR_1) mustBe None
      FORMATTER.getRootCause(EXCEPTION_WITH_ERROR_2) mustBe None
    }

    "returning the root cause message" should {
      "always return the correct and deepest root cause message when a root cause is available" in new CauseFixture {
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_CAUSE_1, None) mustBe DEEPEST_CAUSE_MESSAGE
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_CAUSE_2, None) mustBe DEEPEST_CAUSE_MESSAGE
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_CAUSE_3, None) mustBe DEEPEST_CAUSE_MESSAGE
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_CAUSE_4, None) mustBe DEEPEST_CAUSE_MESSAGE
      }

      "always return the default message when no root cause is available and no fallback message was not provided" in new CauseFixture {
        FORMATTER.getRootCauseMessage(null, null) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getRootCauseMessage(null, None) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_NO_CAUSE, None) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getRootCauseMessage(EMPTY_EXCEPTION_1, None) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getRootCauseMessage(EMPTY_EXCEPTION_2, None) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getRootCauseMessage(EMPTY_EXCEPTION_3, None) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
      }

      "always return the fallback message when no root cause is available and a fallback message was provided" in new CauseFixture {
        FORMATTER.getRootCauseMessage(null, Some(ROOT_CAUSE_FALLBACK_MESSAGE)) mustBe ROOT_CAUSE_FALLBACK_MESSAGE
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_NO_CAUSE, Some(ROOT_CAUSE_FALLBACK_MESSAGE)) mustBe ROOT_CAUSE_FALLBACK_MESSAGE
        FORMATTER.getRootCauseMessage(EMPTY_EXCEPTION_1, Some(ROOT_CAUSE_FALLBACK_MESSAGE)) mustBe ROOT_CAUSE_FALLBACK_MESSAGE
        FORMATTER.getRootCauseMessage(EMPTY_EXCEPTION_2, Some(ROOT_CAUSE_FALLBACK_MESSAGE)) mustBe ROOT_CAUSE_FALLBACK_MESSAGE
        FORMATTER.getRootCauseMessage(EMPTY_EXCEPTION_3, Some(ROOT_CAUSE_FALLBACK_MESSAGE)) mustBe ROOT_CAUSE_FALLBACK_MESSAGE
      }

      "always return the default message when an error occurs and no fallback message was not provided" in new CauseFixture {
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_ERROR_1, None) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_ERROR_2, None) mustBe ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT
      }

      "always return the fallback message when an error occurs and a fallback message was provided" in new CauseFixture {
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_ERROR_1, Some(ROOT_CAUSE_FALLBACK_MESSAGE)) mustBe ROOT_CAUSE_FALLBACK_MESSAGE
        FORMATTER.getRootCauseMessage(EXCEPTION_WITH_ERROR_2, Some(ROOT_CAUSE_FALLBACK_MESSAGE)) mustBe ROOT_CAUSE_FALLBACK_MESSAGE
      }
    }

    "returning the root cause message as an optional" should {
      "always return the correct and deepest root cause message when a root cause is available" in new CauseFixture {
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_CAUSE_1) mustBe Some(DEEPEST_CAUSE_MESSAGE)
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_CAUSE_2) mustBe Some(DEEPEST_CAUSE_MESSAGE)
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_CAUSE_3) mustBe Some(DEEPEST_CAUSE_MESSAGE)
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_CAUSE_4) mustBe Some(DEEPEST_CAUSE_MESSAGE)
      }

      "always return the default message when no root cause is available and no fallback message was not provided" in new CauseFixture {
        FORMATTER.getRootCauseMessageOpt(null) mustBe None
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_NO_CAUSE) mustBe None
        FORMATTER.getRootCauseMessageOpt(EMPTY_EXCEPTION_1) mustBe None
        FORMATTER.getRootCauseMessageOpt(EMPTY_EXCEPTION_2) mustBe None
        FORMATTER.getRootCauseMessageOpt(EMPTY_EXCEPTION_3) mustBe None
      }

      "always return the fallback message when no root cause is available and a fallback message was provided" in new CauseFixture {
        FORMATTER.getRootCauseMessageOpt(null) mustBe None
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_NO_CAUSE) mustBe None
        FORMATTER.getRootCauseMessageOpt(EMPTY_EXCEPTION_1) mustBe None
        FORMATTER.getRootCauseMessageOpt(EMPTY_EXCEPTION_2) mustBe None
        FORMATTER.getRootCauseMessageOpt(EMPTY_EXCEPTION_3) mustBe None
      }

      "always return the default message when an error occurs and no fallback message was not provided" in new CauseFixture {
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_ERROR_1) mustBe None
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_ERROR_2) mustBe None
      }

      "always return the fallback message when an error occurs and a fallback message was provided" in new CauseFixture {
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_ERROR_1) mustBe None
        FORMATTER.getRootCauseMessageOpt(EXCEPTION_WITH_ERROR_2) mustBe None
      }
    }

    "returning the root cause type" should {
      "always return the correct and deepest root cause type when a root cause is available" in new CauseFixture {
        FORMATTER.getRootCauseType(null, null, null, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(null, ExceptionFormatterUtils.SIMPLE, null, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(null, ExceptionFormatterUtils.CANONICAL, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(null, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Some(DEEPEST_CAUSE_TYPE_SIMPLE)
        FORMATTER.getRootCauseType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Some(DEEPEST_CAUSE_TYPE_CANONICAL)
        FORMATTER.getRootCauseType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe Some(DEEPEST_CAUSE_TYPE_FULL)
        FORMATTER.getRootCauseType(EXCEPTION_WITH_NO_CAUSE, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EMPTY_EXCEPTION_1, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EMPTY_EXCEPTION_2, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EMPTY_EXCEPTION_3, ExceptionFormatterUtils.FULL, None, removeCompanionObjectSuffix = false) mustBe None
      }

      "always return the correct and deepest root cause type when a root cause is available with a fallback if applicable" in new CauseFixture {
        FORMATTER.getRootCauseType(null, null, null, removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(null, ExceptionFormatterUtils.CANONICAL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(null, ExceptionFormatterUtils.FULL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.SIMPLE, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe Some(DEEPEST_CAUSE_TYPE_SIMPLE)
        FORMATTER.getRootCauseType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.CANONICAL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe Some(DEEPEST_CAUSE_TYPE_CANONICAL)
        FORMATTER.getRootCauseType(EXCEPTION_WITH_CAUSE_1, ExceptionFormatterUtils.FULL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe Some(DEEPEST_CAUSE_TYPE_FULL)
        FORMATTER.getRootCauseType(EXCEPTION_WITH_NO_CAUSE, ExceptionFormatterUtils.FULL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EMPTY_EXCEPTION_1, ExceptionFormatterUtils.FULL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EMPTY_EXCEPTION_2, ExceptionFormatterUtils.FULL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe None
        FORMATTER.getRootCauseType(EMPTY_EXCEPTION_3, ExceptionFormatterUtils.FULL, Some(ROOT_CAUSE_CLASS_FALLBACK_NAME), removeCompanionObjectSuffix = false) mustBe None
      }
    }
  }

  trait BaseFixture {
    val FORMATTER: ExceptionRootCauseFormatter = ExceptionRootCauseFormatter.apply
    val FORMATTER2: ExceptionRootCauseFormatter = ExceptionRootCauseFormatter(new Names())
    val EMPTY_MESSAGE_1: String = null
    val DEEPEST_CAUSE_MESSAGE: String = "The bad boy is here"
    val DEEPEST_CAUSE_TYPE_SIMPLE: String = "NullPointerException"
    val DEEPEST_CAUSE_TYPE_CANONICAL: String = "java.lang.NullPointerException"
    val DEEPEST_CAUSE_TYPE_FULL: String = "java.lang.NullPointerException"

    val EXCEPTION: Throwable = new Exception("Base Exception")
    val EMPTY_EXCEPTION_1: Throwable = new Exception(EMPTY_MESSAGE_1)
    val EMPTY_EXCEPTION_2: Throwable = new Exception("")
    val EMPTY_EXCEPTION_3: Throwable = new Exception(" ")
    val DEEPEST_CAUSE: Throwable = new NullPointerException("The bad boy is here")
    val CAUSE_1: Throwable = new IllegalArgumentException("Cause 1", DEEPEST_CAUSE)
    val CAUSE_2: Throwable = new IllegalArgumentException("Cause 2", CAUSE_1)
    val CAUSE_3: Throwable = new RuntimeException("Cause_3", CAUSE_2)
    val CAUSE_4: Throwable = new RuntimeException("Cause_4", CAUSE_3)

    FORMATTER.setNames(new Names())
  }

  trait CauseFixture extends BaseFixture {
    val ROOT_CAUSE_FALLBACK_MESSAGE_DEFAULT: String = "-- No Root Cause Message was available --"
    val ROOT_CAUSE_FALLBACK_MESSAGE: String = "No message there but I am here"
    val ROOT_CAUSE_CLASS_FALLBACK_NAME: String = "Hi, Fallback here"

    class BadException extends Exception() {
      override def getCause: Throwable = throw new Exception("Not gonna cause!")
    }

    class BadException2 extends Exception() {
      override def getMessage: String = throw new Exception("Not gonna message!")
    }

    val EXCEPTION_WITH_ERROR_1: Throwable = new BadException()
    val EXCEPTION_WITH_ERROR_2: Throwable = new BadException2()
    val EXCEPTION_WITH_NO_CAUSE: Throwable = new Exception("Base Exception")
    val EXCEPTION_WITH_CAUSE_1: Throwable = new Exception("Base Exception", CAUSE_1)
    val EXCEPTION_WITH_CAUSE_2: Throwable = new Exception("Base Exception", CAUSE_2)
    val EXCEPTION_WITH_CAUSE_3: Throwable = new Exception("Base Exception", CAUSE_3)
    val EXCEPTION_WITH_CAUSE_4: Throwable = new Exception("Base Exception", CAUSE_4)
  }
}