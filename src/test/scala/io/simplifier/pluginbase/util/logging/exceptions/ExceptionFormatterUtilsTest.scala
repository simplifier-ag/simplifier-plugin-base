package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtils.{CANONICAL, FULL, SIMPLE}
import org.json4s.{JValue, _}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Success

class ExceptionFormatterUtilsTest extends AnyWordSpec with Matchers {

  "An exception formatter util" when {

    "extracting the simple class name without companion object suffix" should {
      "return OBJECT_UNAVAILABLE for a null class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Success(OBJECT_UNAVAILABLE)
      }
      "return the simple class name string for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_STRING_SIMPLE)
      }
      "return the class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MAP_SIMPLE)
      }
      "return the name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_TEST_SIMPLE)
      }
      "return the name for a boxed unit class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_BOXED_UNIT_SIMPLE)
      }
      "return the name for an anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Success("7")
      }
      "return the name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, SIMPLE, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_SIMPLE)
      }
    }

    "extracting the simple class name with companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, SIMPLE, None, removeCompanionObjectSuffix = true) mustBe Success(OBJECT_UNAVAILABLE)
      }
      "return the simple class name string for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, SIMPLE, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_STRING_SIMPLE)
      }
      "return the class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, SIMPLE, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MAP_SIMPLE)
      }
      "return the name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, SIMPLE, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_TEST_SIMPLE)
      }
      "return the name for a boxed unit class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), SIMPLE, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_BOXED_UNIT_SIMPLE)
      }
      "return the name for an anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, SIMPLE, None, removeCompanionObjectSuffix = true) mustBe Success("15")
      }
      "return the name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, SIMPLE, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_SIMPLE_WITHOUT_SUFFIX)
      }
    }

    "failing to extract the simple classname with fallback without companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(FALLBACK)
      }
      "return the simple class name string for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_STRING_SIMPLE)
      }
      "return the class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MAP_SIMPLE)
      }
      "return the name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_TEST_SIMPLE)
      }
      "return the name for a boxed unit class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_BOXED_UNIT_SIMPLE)
      }
      "return the name for an anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success("23")
      }
      "return the name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_SIMPLE)
      }
    }

    "failing to extract the simple classname with fallback with companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(FALLBACK)
      }
      "return the simple class name string for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_STRING_SIMPLE)
      }
      "return the class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MAP_SIMPLE)
      }
      "return the name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_TEST_SIMPLE)
      }
      "return the name for a boxed unit class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_BOXED_UNIT_SIMPLE)
      }
      "return the name for an anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success("31")
      }
      "return the name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, SIMPLE, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_SIMPLE_WITHOUT_SUFFIX)
      }
    }


    "extracting the canonical class name without companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Success(OBJECT_UNAVAILABLE)
      }
      "return the canonical class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_STRING_CANONICAL)
      }
      "return the canonical class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MAP_CANONICAL)
      }
      "return the canonical name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_TEST_CANONICAL)
      }
      "return the canonical name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_BOXED_UNIT_CANONICAL)
      }
      "return the canonical name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_UNAVAILABLE)
      }
      "return the canonical name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, CANONICAL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL)
      }
    }

    "extracting the canonical class name with companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, CANONICAL, None, removeCompanionObjectSuffix = true) mustBe Success(OBJECT_UNAVAILABLE)
      }
      "return the canonical class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, CANONICAL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_STRING_CANONICAL)
      }
      "return the canonical class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, CANONICAL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MAP_CANONICAL)
      }
      "return the canonical name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, CANONICAL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_TEST_CANONICAL)
      }
      "return the canonical name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), CANONICAL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_BOXED_UNIT_CANONICAL)
      }
      "return the canonical name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, CANONICAL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_UNAVAILABLE)
      }
      "return the canonical name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, CANONICAL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL_WITHOUT_SUFFIX)
      }
    }

    "use the fallback if the canonical class name could not be retrieved without companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(FALLBACK)
      }
      "return the canonical class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_STRING_CANONICAL)
      }
      "return the canonical class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MAP_CANONICAL)
      }
      "return the canonical name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_TEST_CANONICAL)
      }
      "return the canonical name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_BOXED_UNIT_CANONICAL)
      }
      "return the canonical name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(FALLBACK)
      }
      "return the canonical name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL)
      }
    }


    "use the fallback if the canonical class name could not be retrieved with companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(FALLBACK)
      }
      "return the canonical class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_STRING_CANONICAL)
      }
      "return the canonical class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MAP_CANONICAL)
      }
      "return the canonical name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_TEST_CANONICAL)
      }
      "return the canonical name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_BOXED_UNIT_CANONICAL)
      }
      "return the canonical name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(FALLBACK)
      }
      "return the canonical name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, CANONICAL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL_WITHOUT_SUFFIX)
      }
    }


    "extracting the full class name without companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, FULL, None, removeCompanionObjectSuffix = false) mustBe Success(OBJECT_UNAVAILABLE)
      }
      "return the full class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, FULL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_STRING_FULL)
      }
      "return the full class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, FULL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MAP_FULL)
      }
      "return the full name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, FULL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_TEST_FULL)
      }
      "return the full name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), FULL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_BOXED_UNIT_FULL)
      }
      "return the full name for a malformed class name" in new ClassNameExtractionFixture {
        UTILS.extractClassName(malformedClassNameInside, FULL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MALFORMED_FULL)
      }
      "return the full name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, FULL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_FULL(71, 72))
      }
      "return the full name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, FULL, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL)
      }
    }

    "extracting the full class name with companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, FULL, None, removeCompanionObjectSuffix = true) mustBe Success(OBJECT_UNAVAILABLE)
      }
      "return the full class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, FULL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_STRING_FULL)
      }
      "return the full class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, FULL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MAP_FULL)
      }
      "return the full name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, FULL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_TEST_FULL)
      }
      "return the full name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), FULL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_BOXED_UNIT_FULL)
      }
      "return the full name for a malformed class name" in new ClassNameExtractionFixture {
        UTILS.extractClassName(malformedClassNameInside, FULL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MALFORMED_FULL)
      }
      "return the full name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, FULL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_FULL(80, 81))
      }
      "return the full name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, FULL, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL_WITHOUT_SUFFIX)
      }
    }

    "use the fallback if the full class name could not be retrieved without companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(FALLBACK)
      }
      "return the full class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_STRING_FULL)
      }
      "return the full class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MAP_FULL)
      }
      "return the full name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_TEST_FULL)
      }
      "return the full name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_BOXED_UNIT_FULL)
      }
      "return the full name for a malformed class name" in new ClassNameExtractionFixture {
        UTILS.extractClassName(malformedClassNameInside, FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_MALFORMED_FULL)
      }
      "return the full name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_FULL(89, 90))
      }
      "return the full name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, FULL, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL)
      }
    }

    "use the fallback if the full class name could not be retrieved with companion object suffix" should {
      "handle null values" in new ClassNameExtractionFixture {
        UTILS.extractClassName(null, FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(FALLBACK)
      }
      "return the full class name for a string object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_STRING_FULL)
      }
      "return the full class name for a map object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(MAP_OBJECT, FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MAP_FULL)
      }
      "return the full name for a class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(test, FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_TEST_FULL)
      }
      "return the full name for a boxed unit" in new ClassNameExtractionFixture {
        UTILS.extractClassName(boxedUnit(), FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_BOXED_UNIT_FULL)
      }
      "return the full name for a malformed class name" in new ClassNameExtractionFixture {
        UTILS.extractClassName(malformedClassNameInside, FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_MALFORMED_FULL)
      }
      "return the full name for a anonymous class" in new ClassNameExtractionFixture {
        UTILS.extractClassName(new ExceptionFormatterUtilsTestAnonClass {}, FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_FULL(98, 99))
      }
      "return the full name for an anonymous object" in new ClassNameExtractionFixture {
        UTILS.extractClassName(ExceptionFormatterUtilsTestAnonClass, FULL, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_ANONYMOUS_COMPANION_FULL_WITHOUT_SUFFIX)
      }
    }

    "extracting any classname" should {
      "always return the correct classname, when an unknown classname type is requested" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, null, None, removeCompanionObjectSuffix = false) mustBe Success(CLASSNAME_TYPE_UNAVAILABLE)
        UTILS.extractClassName(STRING_OBJECT, null, Some(FALLBACK), removeCompanionObjectSuffix = false) mustBe Success(FALLBACK)
      }

      "always return the classname without the companion object suffix, when requested" in new ClassNameExtractionFixture {
        UTILS.extractClassName(STRING_OBJECT, null, None, removeCompanionObjectSuffix = true) mustBe Success(CLASSNAME_TYPE_UNAVAILABLE)
        UTILS.extractClassName(STRING_OBJECT, null, Some(FALLBACK), removeCompanionObjectSuffix = true) mustBe Success(FALLBACK)
      }
    }

    "extracting the message without a custom fallback" should {
      "always return the correct message" in new MessageExtraction {
        UTILS.retrieveMessage(null, None, DEFAULT_FALLBACK) mustBe DEFAULT_FALLBACK
        UTILS.retrieveMessage(new Error(), None, DEFAULT_FALLBACK) mustBe DEFAULT_FALLBACK
        UTILS.retrieveMessage(new Error2(), None, DEFAULT_FALLBACK) mustBe DEFAULT_FALLBACK
        UTILS.retrieveMessage(ERROR, None, DEFAULT_FALLBACK) mustBe ERROR.getMessage
      }
    }

    "extracting the message with a custom fallback" should {
      "always return the correct message" in new MessageExtraction {
        UTILS.retrieveMessage(null, Some(CUSTOM_FALLBACK), DEFAULT_FALLBACK) mustBe CUSTOM_FALLBACK
        UTILS.retrieveMessage(ERROR2, Some(CUSTOM_FALLBACK), DEFAULT_FALLBACK) mustBe CUSTOM_FALLBACK
        UTILS.retrieveMessage(ERROR3, Some(CUSTOM_FALLBACK), DEFAULT_FALLBACK) mustBe CUSTOM_FALLBACK
        UTILS.retrieveMessage(new Error(), Some(CUSTOM_FALLBACK), DEFAULT_FALLBACK) mustBe CUSTOM_FALLBACK
        UTILS.retrieveMessage(new Error2(), Some(CUSTOM_FALLBACK), DEFAULT_FALLBACK) mustBe CUSTOM_FALLBACK
      }
    }
  }

  trait BaseFixture {
    val UTILS: ExceptionFormatterUtils = ExceptionFormatterUtils.apply
    val UTILS2: ExceptionFormatterUtils = ExceptionFormatterUtils(new Names())
    UTILS.setNames(new Names())
  }

  trait Prettyfying extends BaseFixture {
    val details: JValue = JObject(
      JField("Pretty-Details-Here", JNull),
      JField("Not-So-pretty-details-Here", JNull),
      JField("camlCaseIsNice", JNull),
      JField("camlCase-IsNice", JNull),
      JField("CamlCase-IsNice", JNull),
      JField("I_LIKE_SNEKS", JNull),
      JField("_i_like_sneks", JNull),
      JField("mixedBag-Here_Yes", JNull),
      JField("-MIXEDixed_BAG-Here_Yes-BAG", JNull),
      JField("-_-_", JNull)
    )

    val detailsPretty: JValue = JObject(
      JField("Pretty-Details-Here", JNull),
      JField("Not-So-Pretty-Details-Here", JNull),
      JField("Caml-Case-Is-Nice", JNull),
      JField("Caml-Case-Is-Nice", JNull),
      JField("Caml-Case-Is-Nice", JNull),
      JField("I-LIKE-SNEKS", JNull),
      JField("-I-Like-Sneks", JNull),
      JField("Mixed-Bag-Here-Yes", JNull),
      JField("-MIXEDixed-BAG-Here-Yes-BAG", JNull),
      JField("-_-_", JNull)
    )
  }

  trait Camelizing extends BaseFixture {
    val details: JValue = JObject(
      JField("Pretty-Details-Here", JNull),
      JField("Not-So-pretty-details-Here", JNull),
      JField("camlCaseIsNice", JNull),
      JField("camlCase-IsNice", JNull),
      JField("CamlCase-IsNice", JNull),
      JField("I-LIKE-SNEKS", JNull),
      JField("I_LIKE_SNEKS", JNull),
      JField("_i_like_sneks", JNull),
      JField("mixedBag-Here_Yes", JNull),
      JField("-MIXEDixed_BAG-Here_Yes-BAG", JNull),
      JField("-_-_", JNull)
    )

    val detailsPretty: JValue = JObject(
      JField("prettyDetailsHere", JNull),
      JField("notSoPrettyDetailsHere", JNull),
      JField("camlCaseIsNice", JNull),
      JField("camlCaseIsNice", JNull),
      JField("camlCaseIsNice", JNull),
      JField("iLikeSneks", JNull),
      JField("iLIKESNEKS", JNull),
      JField("iLikeSneks", JNull),
      JField("mixedBagHereYes", JNull),
      JField("mIXEDixedBAGHereYesBAG", JNull),
      JField("-_-_", JNull)
    )
  }

  trait Snakifying extends BaseFixture {
    val details: JValue = JObject(
      JField("Pretty-Details-Here", JNull),
      JField("Not-So-pretty-details-Here", JNull),
      JField("camlCaseIsNice", JNull),
      JField("camlCase-IsNice", JNull),
      JField("CamlCase-IsNice", JNull),
      JField("I_LIKE_SNEKS", JNull),
      JField("_i_like_sneks", JNull),
      JField("mixedBag-Here_Yes", JNull),
      JField("-MIXEDixed_BAG-Here_Yes-BAG", JNull),
      JField("-_-_", JNull)
    )

    val detailsPretty: JValue = JObject(
      JField("pretty_details_here", JNull),
      JField("not_so_pretty_details_here", JNull),
      JField("caml_case_is_nice", JNull),
      JField("caml_case_is_nice", JNull),
      JField("caml_case_is_nice", JNull),
      JField("i_like_sneks", JNull),
      JField("_i_like_sneks", JNull),
      JField("mixed_bag_here_yes", JNull),
      JField("_mixe_dixed_bag_here_yes_bag", JNull),
      JField("-_-_", JNull)
    )
  }

  trait ClassNameExtractionFixture extends BaseFixture {

    val FALLBACK: String = "This is a fallback"

    val OBJECT_UNAVAILABLE: String = "-- The object has not been provided --"
    val CLASSNAME_UNAVAILABLE = "-- The desired class name has not been provided --"
    val CLASSNAME_TYPE_UNAVAILABLE = "-- The desired class name type has not been provided --"

    val STRING_OBJECT: String = "string"
    val MAP_OBJECT: Map[Int, Any] = Map(1 -> "", 3 -> "")

    val CLASSNAME_STRING_SIMPLE: String = "String"
    val CLASSNAME_STRING_CANONICAL: String = "java.lang.String"
    val CLASSNAME_STRING_FULL: String = "java.lang.String"

    val CLASSNAME_MAP_SIMPLE: String = "Map2"
    val CLASSNAME_MAP_CANONICAL: String = "scala.collection.immutable.Map.Map2"
    val CLASSNAME_MAP_FULL: String = "scala.collection.immutable.Map$Map2"

    class Test {
      def hi: String = "hi"

      override def toString: String = "I am test baby"
    }

    def test = new Test

    object MalformedClassNameInside {

      class Test2 extends Test

    }

    class Test3 extends Test {
      override def hi: String = "bye"
    }

    def boxedUnit(): Unit = {
      class Test2 {}
    }

    def malformedClassNameInside = new MalformedClassNameInside.Test2

    val CLASSNAME_TEST_SIMPLE: String = "Test"
    val CLASSNAME_TEST_CANONICAL: String = "io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTest.ClassNameExtractionFixture.Test"
    val CLASSNAME_TEST_FULL: String = "io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTest$ClassNameExtractionFixture$Test"

    val CLASSNAME_BOXED_UNIT_SIMPLE: String = "BoxedUnit"
    val CLASSNAME_BOXED_UNIT_CANONICAL: String = "scala.runtime.BoxedUnit"
    val CLASSNAME_BOXED_UNIT_FULL: String = "scala.runtime.BoxedUnit"

    val CLASSNAME_MALFORMED_SIMPLE: String = "io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTest$ClassNameExtractionFixture$MalformedClassNameInside$Test2"
    val CLASSNAME_MALFORMED_CANONICAL: String = "io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTest$ClassNameExtractionFixture$MalformedClassNameInside$Test2"
    val CLASSNAME_MALFORMED_FULL: String = "io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTest$ClassNameExtractionFixture$MalformedClassNameInside$Test2"

    def CLASSNAME_ANONYMOUS_SIMPLE(number: Int): String = s"$$anon$$$number"

    val CLASSNAME_ANONYMOUS_CANONICAL: String = CLASSNAME_UNAVAILABLE

    def CLASSNAME_ANONYMOUS_FULL(number1: Int,
                                 number2: Int): String = s"io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTest$$$$anon$$$number1$$$$anon$$$number2"

    val CLASSNAME_ANONYMOUS_COMPANION_SIMPLE: String = "ExceptionFormatterUtilsTestAnonClass"
    val CLASSNAME_ANONYMOUS_COMPANION_SIMPLE_WITHOUT_SUFFIX: String = "ExceptionFormatterUtilsTestAnonClass"


    val CLASSNAME_ANONYMOUS_COMPANION_FULL: String = "io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTestAnonClass$"
    val CLASSNAME_ANONYMOUS_COMPANION_FULL_WITHOUT_SUFFIX: String = "io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtilsTestAnonClass" +
      ""

    val MALFORMED_ERROR: InternalError = new InternalError("Malformed class name")
  }

  trait MessageExtraction extends BaseFixture {
    val DEFAULT_FALLBACK: String = "Default"
    val CUSTOM_FALLBACK: String = "Custom"
    val ERROR: Exception = new Exception("Exception here")
    val ERROR2: Exception = new Exception("")
    val ERROR3: Exception = new Exception("    ")

    class Error extends Throwable {
      override def getMessage: String = null
    }

    class Error2 extends Throwable {
      override def getMessage: String = throw new Exception("Hello")
    }

  }

}