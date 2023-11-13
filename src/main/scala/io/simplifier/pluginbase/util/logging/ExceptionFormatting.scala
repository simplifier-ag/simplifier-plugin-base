package io.simplifier.pluginbase.util.logging

import io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtils.{CANONICAL, ClassNameType, FULL, SIMPLE}
import io.simplifier.pluginbase.util.logging.exceptions.{ExceptionFormatter, ExceptionFormatterUtils, ExceptionRootCauseFormatter}

/**
  * Trait providing operations for safe exception/root cause/message retrieval and detail creation.
  */
trait ExceptionFormatting {

  import ExceptionFormatting._

  private[this] val EXCEPTION_FORMATTER_UTILS: ExceptionFormatterUtils = ExceptionFormatterUtils.apply
  private[this] val EXCEPTION_FORMATTER: ExceptionFormatter = ExceptionFormatter.apply
  private[this] val EXCEPTION_ROOT_CAUSE_FORMATTER: ExceptionRootCauseFormatter = ExceptionRootCauseFormatter.apply

  /** Class names will be displayed in the simple form. */
  def SimpleClassNames: ClassNameType = SIMPLE

  /** Class names will be displayed in the canonical form. */
  def CanonicalClassNames: ClassNameType = CANONICAL

  /** Class names will be displayed in the full form. */
  def FullClassNames: ClassNameType = FULL

  /**
    * Returns safely the type of the exception.
    *
    * @note                                 any occurring errors that might occur during the extraction process are ignored and will not be propagated,
    *                                       as this method is safe. Also if the parameters are <b>null</b> fallback strings or an optional provided fallback name will be used.
    * @param error                          the provided exception.
    * @param errorFallback                  the name that should be rendered into the message when no exception type could be retrieved. The default is [[EXCEPTION_ROOT_CAUSE_TYPE_NOT_RETRIEVABLE]].
    * @param classNameType                  the type of the class name that should be rendered into the message. The default is [[FULL]].
    * @param classNameFallback              the optional name that should be returned as a class name, when the class name is null or not retrievable.
    *                                       The default is <b>None</b> and such the respective error message will be returned instead of a class name.
    * @param removeCompanionObjectSuffix    the flag that determines, whether the companion object suffix <b>$</b> at the end of the classname should be removed or not.
    *                                       The default is <b>true</b> and companion object suffixes <b>$</b> will be removed.
    *
    * @return                               the exception type as a string.
    */
  def getExceptionTypeSafely(error: Throwable,
                             errorFallback: String = EXCEPTION_TYPE_NOT_RETRIEVABLE,
                             classNameType: ClassNameType = FullClassNames,
                             classNameFallback: Option[String] = None,
                             removeCompanionObjectSuffix: Boolean = true): String = {
    EXCEPTION_FORMATTER
      .getExceptionType(error, classNameType, classNameFallback, removeCompanionObjectSuffix)
      .getOrElse(errorFallback)
  }

  /**
    * Returns safely the message of the exception.
    *
    * @note                     any occurring errors that might occur during the extraction process are ignored and will not be propagated,
    *                           as this method is safe. Also if the parameters are <b>null</b> fallback strings will be used.
    * @param error              the provided exception.
    * @param fallbackMessage    the fallback message that should be used when the message itself is <b>null</b> or empty.
    *
    * @return                   the exception's message.
    */
  def getExceptionMessageSafely(error: Throwable,
                                fallbackMessage: String = EXCEPTION_MESSAGE_NOT_AVAILABLE): String = {
    EXCEPTION_FORMATTER.
      getExceptionMessage(error, Option(fallbackMessage).filter(_.nonEmpty))
  }


  /**
    * Returns safely the type of the exception's root cause or the type of the provided exception, when the root cause was null.
    *
    * @note                                 any occurring errors that might occur during the extraction process are ignored and will not be propagated,
    *                                       as this method is safe. Also if the parameters are <b>null</b> fallback strings will be used.
    * @note                                 should the root cause be null, then the type of the provided exception will be retrieved instead.
    *                                       The fallback will apply after the exception type could not be retrieved.
    * @param error                          the provided exception.
    * @param errorFallback                  the fallback message that should be used when the root cause and exception type itself is <b>null</b> or empty.
    *                                       The default is [[EXCEPTION_ROOT_CAUSE_AND_EXCEPTION_TYPE_NOT_RETRIEVABLE]].
    * @param classNameType                  the type of the class name that should be rendered into the message. The default is [[FULL]].
    * @param classNameFallback              the optional name that should be returned as a class name, when the class name is null or not retrievable.
    *                                       The default is <b>None</b> and such the respective error message will be returned instead of a class name.
    * @param removeCompanionObjectSuffix    the flag that determines, whether the companion object suffix <b>$</b> at the end of the classname should be removed or not.
    *                                       The default is <b>true</b> and companion object suffixes <b>$</b> will be removed.
    *
    * @return                               the exception's root cause or exception type as a string.
    */
  def getCauseTypeSafelyWithFallback(error: Throwable,
                                     errorFallback: String = EXCEPTION_ROOT_CAUSE_AND_EXCEPTION_TYPE_NOT_RETRIEVABLE,
                                     classNameType: ClassNameType = FullClassNames,
                                     classNameFallback: Option[String] = None,
                                     removeCompanionObjectSuffix: Boolean = true): String = {
    EXCEPTION_ROOT_CAUSE_FORMATTER
      .getRootCauseType(error, classNameType, classNameFallback, removeCompanionObjectSuffix)
      .orElse(Option(getExceptionTypeSafely(error, errorFallback, classNameType, classNameFallback, removeCompanionObjectSuffix)).filter(_.trim.nonEmpty))
      .getOrElse(errorFallback)
  }

  /**
    * Returns safely the message of the exception's root cause or the message of the provided exception, when the root cause was null.
    *
    * @note                     any occurring errors that might occur during the extraction process are ignored and will not be propagated,
    *                           as this method is safe. Also if the parameters are <b>null</b> fallback strings will be used.
    * @note                     should the root cause be null, then the message of the provided exception will be retrieved instead.
    *                           The fallback will apply after the exception message could not be retrieved.
    * @param error              the provided exception.
    * @param fallbackMessage    the fallback message that should be used when the root cause and exception message itself is <b>null</b> or empty.
    *                           The default is [[EXCEPTION_ROOT_CAUSE_AND_EXCEPTION_MESSAGE_NOT_RETRIEVABLE]].
    *
    * @return                   the exception's root cause or exception message.
    */
  def getCauseMessageSafelyWithFallback(error: Throwable,
                                        fallbackMessage: String = EXCEPTION_ROOT_CAUSE_AND_EXCEPTION_MESSAGE_NOT_RETRIEVABLE): String = {
    EXCEPTION_ROOT_CAUSE_FORMATTER
      .getRootCauseMessageOpt(error)
      .getOrElse(getExceptionMessageSafely(error, fallbackMessage))
  }


  /**
    * Returns safely the class name for a provided object.
    *
    * @note                                 any occurring Non-Fatal errors that might occur during the extraction process are ignored and will not be propagated, as this method is safe.
    *                                       also if the parameters are <b>null</b> fallback strings will be used, which can be globally overruled by an optional @class
    * @param `object`                       the object itself.
    * @param classNameType                  the type of the class name that should be rendered into the message. The default is [[FULL]].
    * @param classNameFallback              the optional name that should be returned as a class name, when the class name is null or not retrievable.
    *                                       The default is <b>None</b> and such the respective error message will be returned instead of a class name.
    * @param classNameErrorFallback         the name that should be rendered into the message when an error occurred. The default is [[CLASSNAME_NOT_RETRIEVABLE]].
    * @param removeCompanionObjectSuffix    the flag that determines, whether the companion object suffix <b>$</b> at the end of the classname should be removed or not.
    *                                       The default is <b>false</b> and companion object suffixes <b>$</b> will not be removed.
    *
    * @return                               the desired class name as a string.
    */
  def getClassNameSafely(`object`: Any,
                         classNameType: ClassNameType = FullClassNames,
                         classNameFallback: Option[String] = None,
                         classNameErrorFallback: String = CLASSNAME_NOT_RETRIEVABLE,
                         removeCompanionObjectSuffix: Boolean = false): String = {
    EXCEPTION_FORMATTER_UTILS
      .extractClassName(`object`, classNameType, classNameFallback, removeCompanionObjectSuffix)
      .getOrElse(classNameErrorFallback)
  }

}

/**
  * The Exception Formatting Companion Object.
  */
 object ExceptionFormatting extends ExceptionFormatting {
  private[ExceptionFormatting] val CLASSNAME_NOT_RETRIEVABLE: String = "-- The class name was not retrievable --"

  private[ExceptionFormatting] val EXCEPTION_TYPE_NOT_RETRIEVABLE: String = "-- The exception type was not retrievable --"
  private[ExceptionFormatting] val EXCEPTION_ROOT_CAUSE_TYPE_NOT_RETRIEVABLE: String = "-- The exception's root cause type was not retrievable --"
  private[ExceptionFormatting] val EXCEPTION_ROOT_CAUSE_AND_EXCEPTION_TYPE_NOT_RETRIEVABLE: String = "-- Neither the exception's root cause type nor the exception cause type was retrievable --"

  private[ExceptionFormatting] val EXCEPTION_MESSAGE_NOT_AVAILABLE: String = "-- The exception message was not available --"
  private[ExceptionFormatting] val EXCEPTION_ROOT_CAUSE_AND_EXCEPTION_MESSAGE_NOT_RETRIEVABLE: String = "-- Neither the exception's root cause message nor the exception cause message was retrievable --"
}