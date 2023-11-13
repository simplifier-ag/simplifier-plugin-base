package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtils.SIMPLE
import org.json4s.{JValue, _}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExceptionJsonFormatterTest extends AnyWordSpec with Matchers {

  "An exception details formatter" when {
    "returning the details without a details converter" should {
      "always return the correct details" in new DetailsFixtureWithoutDetails {
        FORMATTER.createDetailsForException(EXCEPTION_1, SIMPLE, None, None, None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_1_DETAILS_WITHOUT_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_2, SIMPLE, None, None, None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_2_DETAILS_WITHOUT_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_3, SIMPLE, None, None, None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_3_DETAILS_WITHOUT_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_4, SIMPLE, None, None, None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_4_DETAILS_WITHOUT_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_5, SIMPLE, None, None, None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_5_DETAILS_WITHOUT_DETAILS
      }
    }

    "returning the details with a details converter" should {
      "always return the correct details" in new DetailsFixtureWithDetails {
        FORMATTER.createDetailsForException(EXCEPTION_1, SIMPLE, None, None, Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_1_DETAILS_WITH_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_2, SIMPLE, None, None, Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_2_DETAILS_WITH_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_3, SIMPLE, None, None, Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_3_DETAILS_WITH_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_4, SIMPLE, None, None, Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_4_DETAILS_WITH_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_5, SIMPLE, None, None, Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_5_DETAILS_WITH_DETAILS
      }
    }

    "returning the details with custom details" should {
      "always return the correct details" in new DetailsFixtureWithCustomDetails {
        FORMATTER.createDetailsForException(EXCEPTION_1, SIMPLE, None, Some(CUSTOM_DETAILS), None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_1_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_2, SIMPLE, None, Some(CUSTOM_DETAILS), None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_2_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_3, SIMPLE, None, Some(CUSTOM_DETAILS), None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_3_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_4, SIMPLE, None, Some(CUSTOM_DETAILS), None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_4_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_5, SIMPLE, None, Some(CUSTOM_DETAILS), None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_5_DETAILS_WITH_CUSTOM_DETAILS
      }
    }

    "returning the details with custom details and a details converter" should {
      "always return the correct details" in new DetailsFixtureWithCustomDetails {
        FORMATTER.createDetailsForException(EXCEPTION_1, SIMPLE, None, Some(CUSTOM_DETAILS), Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_1_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_2, SIMPLE, None, Some(CUSTOM_DETAILS), Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_2_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_3, SIMPLE, None, Some(CUSTOM_DETAILS), Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_3_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_4, SIMPLE, None, Some(CUSTOM_DETAILS), Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_4_DETAILS_WITH_CUSTOM_DETAILS
        FORMATTER.createDetailsForException(EXCEPTION_5, SIMPLE, None, Some(CUSTOM_DETAILS), Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_5_DETAILS_WITH_CUSTOM_DETAILS
      }
    }

    "returning the details with a default classname fallback" should {
      "always return the correct details" in new DetailsFixtureWithClassnameFallback {
        FORMATTER.createDetailsForException(null, SIMPLE, Some(FALLBACK), None, None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe EXCEPTION_1_DETAILS_WITHOUT_DETAILS
      }
    }

    "returning the details with non-pretty field names" should {
      "always return the correct details" in new DetailsFixtureWithNonPrettyFieldNames {
        FORMATTER.createDetailsForException(EXCEPTION_1, SIMPLE, None, None, None, prettyFieldNames = false, removeCompanionObjectSuffix = false) mustBe EXCEPTION_1_DETAILS_WITH_NON_PRETTY_FIELD_NAMES
      }
    }

    "returning the details for bad exceptions" should {
      "always return the correct details" in new DetailsErrorDetails {
        FORMATTER.createDetailsForException(FAULTY_EXCEPTION, SIMPLE, None, None, None, prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe FAULTY_EXCEPTION_1_DETAILS
        FORMATTER.createDetailsForException(FAULTY_EXCEPTION, SIMPLE, None, None, Some(detailsConverter), prettyFieldNames = true, removeCompanionObjectSuffix = false) mustBe FAULTY_EXCEPTION_1_DETAILS
      }
    }

  }

  trait BaseFixture {

    class MyException(msg: String, cause: Throwable) extends Exception(msg, cause)

    class MyException2(msg: String, cause: Throwable) extends Exception(msg, cause)

    class MyException3(msg: String, detail1: Int, cause: Throwable) extends Exception(msg, cause) {
      def getDetail1: Int = detail1
    }

    class MyException4(msg: String, detail1: Int, detail2: String, cause: Throwable) extends MyException3(msg, detail1, cause) {
      def getDetail2: String = detail2
    }

    val FORMATTER: ExceptionJsonFormatter = ExceptionJsonFormatter.apply
    val FORMATTER2: ExceptionJsonFormatter = ExceptionJsonFormatter(new Names())
    val EMPTY_MESSAGE_1: String = null
    val MESSAGE_1: String = "Message 1"
    val MESSAGE_2: String = "Message 2"
    val MESSAGE_3: String = "Message 3"
    val MESSAGE_4: String = "Message 4"
    val MESSAGE_5: String = "Message 5"

    val MESSAGE_CAUSE_1: String = "Cause 1"
    val MESSAGE_CAUSE_2: String = "Cause 2"
    val MESSAGE_CAUSE_3: String = "Cause 3"
    val MESSAGE_CAUSE_4: String = "Cause 4"
    val DEEPEST_CAUSE_MESSAGE: String = "The bad boy is here"
    val EXCEPTION_TYPE_SIMPLE: String = "Exception"
    val EXCEPTION_TYPE_CANONICAL: String = "java.lang.Exception"
    val EXCEPTION_TYPE_FULL: String = "java.lang.Exception"

    val EXCEPTION: Throwable = new Exception("Base Exception")
    val EMPTY_EXCEPTION_1: Throwable = new Exception(EMPTY_MESSAGE_1)
    val EMPTY_EXCEPTION_2: Throwable = new Exception("")
    val DEEPEST_CAUSE: Throwable = new NullPointerException(DEEPEST_CAUSE_MESSAGE)
    val CAUSE_1: Throwable = new IllegalArgumentException(MESSAGE_CAUSE_1, DEEPEST_CAUSE)
    val CAUSE_2: Throwable = new IllegalArgumentException(MESSAGE_CAUSE_2, CAUSE_1)
    val CAUSE_3: Throwable = new RuntimeException(MESSAGE_CAUSE_3, CAUSE_2)
    val CAUSE_4: Throwable = new RuntimeException(MESSAGE_CAUSE_4, CAUSE_3)

    FORMATTER.setNames(new Names())
  }

  trait DetailsFixtureWithoutDetails extends BaseFixture {

    def detailsConverter(error: Throwable): JValue = {
      error match {
        case ex: MyException4 => JObject(
          JField("detail1", JInt(ex.getDetail1)),
          JField("detail2", JString(ex.getDetail2))
        )
        case ex: MyException3 => JObject(
          JField("detail1", JInt(ex.getDetail1))
        )
        case _ => JNothing
      }
    }

    val EXCEPTION_1: Throwable = new MyException(MESSAGE_1, null)
    val EXCEPTION_2: Throwable = new MyException2(MESSAGE_2, CAUSE_4)
    val EXCEPTION_3: Throwable = new MyException3(MESSAGE_3, 21, CAUSE_3)
    val EXCEPTION_4: Throwable = new MyException3(MESSAGE_4, 23, CAUSE_2)
    val EXCEPTION_5: Throwable = new MyException4(MESSAGE_5, 42, "leet", CAUSE_4)

    EXCEPTION_2.addSuppressed(DEEPEST_CAUSE)
    EXCEPTION_2.addSuppressed(DEEPEST_CAUSE)
    EXCEPTION_2.addSuppressed(DEEPEST_CAUSE)

    EXCEPTION_5.addSuppressed(CAUSE_1)
    EXCEPTION_5.addSuppressed(CAUSE_2)
    EXCEPTION_5.addSuppressed(CAUSE_3)
    EXCEPTION_5.addSuppressed(EXCEPTION_2)

    val EXCEPTION_1_DETAILS_WITHOUT_DETAILS: JObject = JObject(
      JField("Type", JString("MyException")),
      JField("Message", JString("Message 1")),
      JField("Details", JNothing),
      JField("Cause", JNothing),
      JField("Suppressed-Errors", JNothing)
    )

    val EXCEPTION_2_DETAILS_WITHOUT_DETAILS: JObject = JObject(
      JField("Type", JString("MyException2")),
      JField("Message", JString(MESSAGE_2)),
      JField("Details", JNothing),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_4)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JArray(List(
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        ),
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        ),
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        )
      )))
    )
    val EXCEPTION_3_DETAILS_WITHOUT_DETAILS: JObject = JObject(
      JField("Type", JString("MyException3")),
      JField("Message", JString(MESSAGE_3)),
      JField("Details", JNothing),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_3)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_1)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JNothing)
    )

    val EXCEPTION_4_DETAILS_WITHOUT_DETAILS: JObject = JObject(
      JField("Type", JString("MyException3")),
      JField("Message", JString(MESSAGE_4)),
      JField("Details", JNothing),
      JField("Cause", JObject(
        JField("Type", JString("IllegalArgumentException")),
        JField("Message", JString(MESSAGE_CAUSE_2)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_1)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("NullPointerException")),
            JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
            JField("Details", JNothing),
            JField("Cause", JNothing),
            JField("Suppressed-Errors", JNothing))),
          JField("Suppressed-Errors", JNothing))),
        JField("Suppressed-Errors", JNothing))),
      JField("Suppressed-Errors", JNothing))

    val EXCEPTION_5_DETAILS_WITHOUT_DETAILS: JObject = JObject(
      JField("Type", JString("MyException4")),
      JField("Message", JString(MESSAGE_5)),
      JField("Details", JNothing),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_4)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JArray(List(
        JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_1)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("NullPointerException")),
            JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
            JField("Details", JNothing),
            JField("Cause", JNothing),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_1)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("MyException2")),
          JField("Message", JString(MESSAGE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("RuntimeException")),
            JField("Message", JString(MESSAGE_CAUSE_4)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("RuntimeException")),
              JField("Message", JString(MESSAGE_CAUSE_3)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("IllegalArgumentException")),
                JField("Message", JString(MESSAGE_CAUSE_2)),
                JField("Details", JNothing),
                JField("Cause", JObject(
                  JField("Type", JString("IllegalArgumentException")),
                  JField("Message", JString(MESSAGE_CAUSE_1)),
                  JField("Details", JNothing),
                  JField("Cause", JObject(
                    JField("Type", JString("NullPointerException")),
                    JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                    JField("Details", JNothing),
                    JField("Cause", JNothing),
                    JField("Suppressed-Errors", JNothing))
                  ),
                  JField("Suppressed-Errors", JNothing)
                )),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JArray(
            List(JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            ),
              JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              ),
              JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )))
          )
        )
      ))))
  }

  trait DetailsFixtureWithDetails extends DetailsFixtureWithoutDetails {

    val EXCEPTION_1_DETAILS_WITH_DETAILS: JValue = EXCEPTION_1_DETAILS_WITHOUT_DETAILS
    val EXCEPTION_2_DETAILS_WITH_DETAILS: JValue = EXCEPTION_2_DETAILS_WITHOUT_DETAILS

    val EXCEPTION_3_DETAILS_WITH_DETAILS: JObject = JObject(
      JField("Type", JString("MyException3")),
      JField("Message", JString(MESSAGE_3)),
      JField("Details", JObject(
        JField("detail1", JInt(21)
        )
      )),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_3)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_1)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JNothing)
    )

    val EXCEPTION_4_DETAILS_WITH_DETAILS: JValue = JObject(
      JField("Type", JString("MyException3")),
      JField("Message", JString(MESSAGE_4)),
      JField("Details", JObject(
        JField("detail1", JInt(23)
        )
      )),
      JField("Cause", JObject(
        JField("Type", JString("IllegalArgumentException")),
        JField("Message", JString(MESSAGE_CAUSE_2)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_1)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("NullPointerException")),
            JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
            JField("Details", JNothing),
            JField("Cause", JNothing),
            JField("Suppressed-Errors", JNothing))),
          JField("Suppressed-Errors", JNothing))),
        JField("Suppressed-Errors", JNothing))),
      JField("Suppressed-Errors", JNothing))

    val EXCEPTION_5_DETAILS_WITH_DETAILS: JValue = JObject(
      JField("Type", JString("MyException4")),
      JField("Message", JString(MESSAGE_5)),
      JField("Details", JObject(
        JField("detail1", JInt(42)),
        JField("detail2", JString("leet"))
      )),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_4)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JArray(List(
        JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_1)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("NullPointerException")),
            JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
            JField("Details", JNothing),
            JField("Cause", JNothing),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_1)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("MyException2")),
          JField("Message", JString(MESSAGE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("RuntimeException")),
            JField("Message", JString(MESSAGE_CAUSE_4)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("RuntimeException")),
              JField("Message", JString(MESSAGE_CAUSE_3)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("IllegalArgumentException")),
                JField("Message", JString(MESSAGE_CAUSE_2)),
                JField("Details", JNothing),
                JField("Cause", JObject(
                  JField("Type", JString("IllegalArgumentException")),
                  JField("Message", JString(MESSAGE_CAUSE_1)),
                  JField("Details", JNothing),
                  JField("Cause", JObject(
                    JField("Type", JString("NullPointerException")),
                    JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                    JField("Details", JNothing),
                    JField("Cause", JNothing),
                    JField("Suppressed-Errors", JNothing))
                  ),
                  JField("Suppressed-Errors", JNothing)
                )),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JArray(
            List(JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            ),
              JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              ),
              JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )))
          )
        )
      ))))
  }




  trait DetailsFixtureWithCustomDetails extends DetailsFixtureWithoutDetails {

    val CUSTOM_DETAILS:JValue = JString("I am totally custom")


    val EXCEPTION_1_DETAILS_WITH_CUSTOM_DETAILS: JObject = JObject(
      JField("Type", JString("MyException")),
      JField("Message", JString("Message 1")),
      JField("Details", CUSTOM_DETAILS),
      JField("Cause", JNothing),
      JField("Suppressed-Errors", JNothing)
    )

    val EXCEPTION_2_DETAILS_WITH_CUSTOM_DETAILS: JObject = JObject(
      JField("Type", JString("MyException2")),
      JField("Message", JString(MESSAGE_2)),
      JField("Details", CUSTOM_DETAILS),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_4)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JArray(List(
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        ),
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        ),
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        )
      )))
    )

    val EXCEPTION_3_DETAILS_WITH_CUSTOM_DETAILS: JObject = JObject(
      JField("Type", JString("MyException3")),
      JField("Message", JString(MESSAGE_3)),
      JField("Details", CUSTOM_DETAILS),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_3)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_1)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JNothing)
    )

    val EXCEPTION_4_DETAILS_WITH_CUSTOM_DETAILS: JValue = JObject(
      JField("Type", JString("MyException3")),
      JField("Message", JString(MESSAGE_4)),
      JField("Details", CUSTOM_DETAILS),
      JField("Cause", JObject(
        JField("Type", JString("IllegalArgumentException")),
        JField("Message", JString(MESSAGE_CAUSE_2)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_1)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("NullPointerException")),
            JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
            JField("Details", JNothing),
            JField("Cause", JNothing),
            JField("Suppressed-Errors", JNothing))),
          JField("Suppressed-Errors", JNothing))),
        JField("Suppressed-Errors", JNothing))),
      JField("Suppressed-Errors", JNothing))




    val EXCEPTION_5_DETAILS_WITH_CUSTOM_DETAILS: JValue = JObject(
      JField("Type", JString("MyException4")),
      JField("Message", JString(MESSAGE_5)),
      JField("Details", CUSTOM_DETAILS),
      JField("Cause", JObject(
        JField("Type", JString("RuntimeException")),
        JField("Message", JString(MESSAGE_CAUSE_4)),
        JField("Details", JNothing),
        JField("Cause", JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        )),
        JField("Suppressed-Errors", JNothing)
      )),
      JField("Suppressed-Errors", JArray(List(
        JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_1)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("NullPointerException")),
            JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
            JField("Details", JNothing),
            JField("Cause", JNothing),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("IllegalArgumentException")),
          JField("Message", JString(MESSAGE_CAUSE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_1)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("RuntimeException")),
          JField("Message", JString(MESSAGE_CAUSE_3)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("IllegalArgumentException")),
            JField("Message", JString(MESSAGE_CAUSE_2)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("IllegalArgumentException")),
              JField("Message", JString(MESSAGE_CAUSE_1)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JNothing)
        ), JObject(
          JField("Type", JString("MyException2")),
          JField("Message", JString(MESSAGE_2)),
          JField("Details", JNothing),
          JField("Cause", JObject(
            JField("Type", JString("RuntimeException")),
            JField("Message", JString(MESSAGE_CAUSE_4)),
            JField("Details", JNothing),
            JField("Cause", JObject(
              JField("Type", JString("RuntimeException")),
              JField("Message", JString(MESSAGE_CAUSE_3)),
              JField("Details", JNothing),
              JField("Cause", JObject(
                JField("Type", JString("IllegalArgumentException")),
                JField("Message", JString(MESSAGE_CAUSE_2)),
                JField("Details", JNothing),
                JField("Cause", JObject(
                  JField("Type", JString("IllegalArgumentException")),
                  JField("Message", JString(MESSAGE_CAUSE_1)),
                  JField("Details", JNothing),
                  JField("Cause", JObject(
                    JField("Type", JString("NullPointerException")),
                    JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                    JField("Details", JNothing),
                    JField("Cause", JNothing),
                    JField("Suppressed-Errors", JNothing))
                  ),
                  JField("Suppressed-Errors", JNothing)
                )),
                JField("Suppressed-Errors", JNothing)
              )),
              JField("Suppressed-Errors", JNothing)
            )),
            JField("Suppressed-Errors", JNothing)
          )),
          JField("Suppressed-Errors", JArray(
            List(JObject(
              JField("Type", JString("NullPointerException")),
              JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
              JField("Details", JNothing),
              JField("Cause", JNothing),
              JField("Suppressed-Errors", JNothing)
            ),
              JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              ),
              JObject(
                JField("Type", JString("NullPointerException")),
                JField("Message", JString(DEEPEST_CAUSE_MESSAGE)),
                JField("Details", JNothing),
                JField("Cause", JNothing),
                JField("Suppressed-Errors", JNothing)
              )))
          )
        )
      ))))
  }




  trait DetailsFixtureWithClassnameFallback extends BaseFixture {

    FORMATTER.setNames(new Names())

    val FALLBACK: String = "Fallback Class"

    val EXCEPTION_1_DETAILS_WITHOUT_DETAILS: JObject =
      JObject(
        JField("Type", JString(FALLBACK)),
        JField("Message", JString("-")),
        JField("Details", JNothing),
        JField("Cause", JNothing),
        JField("Suppressed-Errors", JNothing)
      )
  }

  trait DetailsFixtureWithNonPrettyFieldNames extends BaseFixture {

    FORMATTER.setNames(new Names())

    val EXCEPTION_1: Throwable = new MyException(MESSAGE_1, null)
    val EXCEPTION_1_DETAILS_WITH_NON_PRETTY_FIELD_NAMES: JObject =
      JObject(
        JField("type", JString("MyException")),
        JField("message", JString(MESSAGE_1)),
        JField("details", JNothing),
        JField("cause", JNothing),
        JField("suppressedErrors", JNothing)
      )
  }

  trait DetailsErrorDetails extends BaseFixture {

    class FaultyException(cause: Throwable) extends Exception("Faulty Baby", cause) {
      override def getCause: Throwable = throw new Exception("No Cause Baby")

      def getDetail: Int = throw new Exception("Nope")
    }

    def detailsConverter(error: Throwable): JValue = {
      error match {
        case ex: FaultyException => JInt(ex.getDetail)
        case _ => JNothing
      }
    }

    val FAULTY_CAUSE_1: Throwable = new NullPointerException(null)
    val FAULTY_CAUSE_2: Throwable = new NullPointerException("Null")
    val FAULTY_EXCEPTION: Throwable = new FaultyException(FAULTY_CAUSE_1)

    FAULTY_EXCEPTION.addSuppressed(FAULTY_CAUSE_1)
    FAULTY_EXCEPTION.addSuppressed(FAULTY_CAUSE_2)

    val FAULTY_EXCEPTION_1_DETAILS: JObject = JObject(
      JField("Type", JString("FaultyException")),
      JField("Message", JString("Faulty Baby")),
      JField("Details", JNothing),
      JField("Cause", JNothing),
      JField("Suppressed-Errors", JArray(List(
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString("-")),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        ),
        JObject(
          JField("Type", JString("NullPointerException")),
          JField("Message", JString("Null")),
          JField("Details", JNothing),
          JField("Cause", JNothing),
          JField("Suppressed-Errors", JNothing)
        ))))
    )
  }

}