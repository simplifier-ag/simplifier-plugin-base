package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtils.ClassNameType

import scala.util.{Failure, Success, Try}


/**
  * The Exception Root Cause Formatter. Containing all formatting methods related to the root cause of an exception.
  *
  * @param names    the [[Names]]-Object for Logging purposes.
  */
class ExceptionRootCauseFormatter(implicit names: Names) {

  import ExceptionRootCauseFormatter._

  private[this] val EXCEPTION_FORMATTER_UTILS: ExceptionFormatterUtils = ExceptionFormatterUtils(names)

  import EXCEPTION_FORMATTER_UTILS._

  /**
    * Updates the name object for logging purposes.
    *
    * @param names    the current [[Names]]-Object.
    */
  def setNames(names: Names): Unit = EXCEPTION_FORMATTER_UTILS.setNames(names)

  /**
    * Returns the root cause for a provided error.
    *
    * @param error    the provided error.
    *
    * @return         the respective root cause.
    */
  protected[logging] def getRootCause(error: Throwable): Option[Throwable] = {
    getRootCauseWorkflow(error)
  }

  /**
    * Returns the root cause's type for a provided error.
    *
    * @param error                          the provided error.
    * @param classNameType                  the type of the class name: [[ClassNameType]].
    * @param classNameFallback              the optional fallback name for a class name, when it is null or not retrievable.
    * @param removeCompanionObjectSuffix    the flag that determines, whether the companion object suffix <b>$</b> at the end of the classname should be removed or not.
    *
    * @return                               the root cause's type or None.
    */
  protected[logging] def getRootCauseType(error: Throwable,
                                          classNameType: ClassNameType,
                                          classNameFallback: Option[String],
                                          removeCompanionObjectSuffix: Boolean): Option[String] = {
    getRootCause(error)
      .flatMap { rootCause =>
        extractClassName(rootCause, classNameType, classNameFallback, removeCompanionObjectSuffix)
          .recoverWith { case e => logger.warn(ErrorDuringRootCauseTypeDetermination, e); Failure(e) }
          .toOption
      }
  }

  /**
    * Returns the root cause's messages for a provided error.
    *
    * @note                     when the message itself is null or empty, then a fallback message will be returned if it is provided.
    *                           Else a default message, telling that the root cause did not contain a message will be returned.
    * @param error              the provided error.
    * @param fallbackMessage    the optional fallback message.
    *
    * @return                   the root cause's message.
    */
  protected[logging] def getRootCauseMessage(error: Throwable,
                                             fallbackMessage: Option[String]): String = {
    (getRootCause(error), fallbackMessage) match {
      case (None, None) | (None, null) => NO_ROOT_CAUSE_MESSAGE_AVAILABLE
      case (None, Some(fallback)) => fallback
      case (Some(null), _) => NO_ROOT_CAUSE_MESSAGE_AVAILABLE
      case (Some(rootCause), _) => retrieveMessage(rootCause, fallbackMessage, NO_ROOT_CAUSE_MESSAGE_AVAILABLE)
    }
  }

  /**
    * Returns the root cause's messages for a provided error as an option.
    *
    * @param error    the provided error.
    *
    * @return         the root cause's message as an option..
    */
  protected[logging] def getRootCauseMessageOpt(error: Throwable): Option[String] = {
    getRootCause(error) match {
      case None | null => None
      case Some(null) => None
      case Some(rootCause) => Option(retrieveMessage(rootCause, None, EMPTY_ROOT_CAUSE_MESSAGE)).filter(_.trim.nonEmpty)
    }
  }

  private[this] def getRootCauseWorkflow(error: Throwable): Option[Throwable] = {
    Try(error match {
      case null => None
      case e if e.getCause != null && e.getCause.getCause == null => Some(e.getCause)
      case e if e.getCause != null && e.getCause.getCause != null => getRootCause(error.getCause)
      case _ => None
    }).recoverWith { case e => logger.warn(ErrorDuringRootCauseDetermination, e); Success(None) }
      .getOrElse(None)
  }
}

/**
  * The Exception Root Cause Formatter Companion Object.
  */
object ExceptionRootCauseFormatter {

  /**
    * Creates the Exception Root Cause Formatter without a provided names object.
    *
    * @return    the [[ExceptionRootCauseFormatter]].
    */
  def apply: ExceptionRootCauseFormatter = new ExceptionRootCauseFormatter()(new Names())

  /**
    * Creates the Exception Root Cause Formatter with a provided names object.
    *
    * @param names    the [[Names]]-object.
    *
    * @return    the [[ExceptionRootCauseFormatter]].
    */
  def apply(names: Names) = new ExceptionRootCauseFormatter()(names)

  private[ExceptionRootCauseFormatter] val NO_ROOT_CAUSE_MESSAGE_AVAILABLE: String = "-- No Root Cause Message was available --"
  private[ExceptionRootCauseFormatter] val EMPTY_ROOT_CAUSE_MESSAGE: String = " "

  private[ExceptionRootCauseFormatter] def ErrorDuringRootCauseDetermination: String = "The root cause could not have been determined, therefore the result will be None"

  private[ExceptionRootCauseFormatter] def ErrorDuringRootCauseTypeDetermination: String = "The root cause type could not have been determined, therefore the result will be None"
}