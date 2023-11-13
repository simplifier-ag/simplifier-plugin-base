package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtils.ClassNameType

import scala.util.Failure


/**
  * The Exception Formatter. Containing all formatting methods related to an exception.
  *
  * @param names    the [[Names]]-Object for Logging purposes.
  */
class ExceptionFormatter(implicit names: Names) {

  import ExceptionFormatter._

  private[this] val EXCEPTION_FORMATTER_UTILS: ExceptionFormatterUtils = ExceptionFormatterUtils(names)

  import EXCEPTION_FORMATTER_UTILS._

  /**
    * Updates the name object for logging purposes.
    *
    * @param names    the current [[Names]]-Object.
    */
  def setNames(names: Names): Unit = EXCEPTION_FORMATTER_UTILS.setNames(names)

  /**
    * Returns the exception's type for a provided error.
    *
    * @param error                          the provided error.
    * @param classNameType                  the type of the class name: [[ClassNameType]].
    * @param classNameFallback              the optional fallback name for a class name, when it is null or not retrievable.
    * @param removeCompanionObjectSuffix    the flag that determines, whether the companion object suffix <b>$</b> at the end of the classname should be removed or not.
    *
    * @return                               the exceptions' type or None.
    */
  protected[logging] def getExceptionType(error: Throwable,
                                          classNameType: ClassNameType,
                                          classNameFallback: Option[String],
                                          removeCompanionObjectSuffix: Boolean): Option[String] = {
    Option(error)
      .flatMap { exception =>
        extractClassName(exception, classNameType, classNameFallback, removeCompanionObjectSuffix)
          .recoverWith { case e => logger.warn(ErrorDuringExceptionTypeDetermination, e); Failure(e) }
          .toOption
      }.orElse(classNameFallback)
  }

  /**
    * Returns the exception's messages for a provided error.
    *
    * @note                     when the message itself is null or empty, then a fallback message will be returned if it is provided.
    *                           Else a default message, telling that the exception did not contain a message will be returned.
    * @param error              the provided error.
    * @param fallbackMessage    the optional fallback message.
    *
    * @return                   the exception's message.
    */
  protected[logging] def getExceptionMessage(error: Throwable,
                                             fallbackMessage: Option[String]): String = {
    (Option(error), fallbackMessage) match {
      case (None, None) | (None, null) => NO_EXCEPTION_MESSAGE_AVAILABLE
      case (None, Some(fallback)) => fallback
      case (Some(null), _) => NO_EXCEPTION_MESSAGE_AVAILABLE
      case (Some(e), _) => retrieveMessage(e, fallbackMessage, NO_EXCEPTION_MESSAGE_AVAILABLE)
    }
  }
}

/**
  * The Exception Formatter Companion Object.
  */
object ExceptionFormatter {

  /**
    * Creates the Exception Formatter without a provided names object.
    *
    * @return    the [[ExceptionFormatter]].
    */
  def apply: ExceptionFormatter = new ExceptionFormatter()(new Names())

  /**
    * Creates the Exception Formatter with a provided names object.
    *
    * @param names    the [[Names]]-object.
    *
    * @return    the [[ExceptionFormatter]].
    */
  def apply(names: Names) = new ExceptionFormatter()(names)

  private[ExceptionFormatter] val NO_EXCEPTION_MESSAGE_AVAILABLE: String = "-- No Exception Message was available --"

  private[ExceptionFormatter] def ErrorDuringExceptionTypeDetermination: String = "The exception type could not have been determined, therefore the result will be None"
}