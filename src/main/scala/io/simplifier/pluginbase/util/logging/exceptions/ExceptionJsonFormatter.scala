package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import io.simplifier.pluginbase.util.logging.exceptions.ExceptionFormatterUtils.ClassNameType
import org.json4s.{JValue, _}

import scala.util.{Failure, Try}


/**
  * The Exception Json Formatter, which contains methods to create a Json representation from a provided exceptions.
  *
  * @param names    the [[Names]]-Object for Logging purposes
  */
class ExceptionJsonFormatter(implicit names: Names) {

  import ExceptionJsonFormatter._

  private[this] val EXCEPTION_FORMATTER_UTILS: ExceptionFormatterUtils = ExceptionFormatterUtils.apply

  protected[logging] val NO_DETAILS_CONVERTER: Throwable => JValue = noDetailsConverter


  /**
    * Updates the name object for logging purposes.
    *
    * @param names    the current [[Names]]-Object.
    */
  def setNames(names: Names): Unit = EXCEPTION_FORMATTER_UTILS.setNames(names)

  /**
    * Creates the details for a provided error.
    *
    * @param error                          the provided error.
    * @param classNameType                  the type of the class name that should be rendered into the message.
    * @param classNameFallback              the optional name that should be returned as a class name, when the class name is null or not retrievable. The default is None.
    * @param details                        the own details to add.
    * @param detailsConverter              the details converter, if no one is provided, then the details will be JNothing.
    * @param prettyFieldNames               the flag, that switches between pretty and camel-case field names.
    * @param removeCompanionObjectSuffix    the flag that determines, whether the companion object suffix <b>$</b> at the end of the classname should be removed or not.
    *
    * @return                               the details for the provided error.
    */
  protected[logging] def createDetailsForException(error: Throwable,
                                                   classNameType: ClassNameType,
                                                   classNameFallback: Option[String],
                                                   details: Option[JValue],
                                                   detailsConverter: Option[Throwable => JValue],
                                                   prettyFieldNames: Boolean,
                                                   removeCompanionObjectSuffix: Boolean): JObject = {
    JObject(
      JField(if (prettyFieldNames) PRETTY_FIELD_NAME_TYPE_STRING else FIELD_NAME_TYPE_STRING, createExceptionTypeJson(error, classNameType, classNameFallback, removeCompanionObjectSuffix)),
      JField(if (prettyFieldNames) PRETTY_FIELD_NAME_MESSAGE_STRING else FIELD_NAME_MESSAGE_STRING, createExceptionMessageJson(error)),
      JField(if (prettyFieldNames) PRETTY_FIELD_NAME_DETAILS_STRING else FIELD_NAME_DETAILS_STRING, createExceptionDetailsJson(error, details, detailsConverter)),
      JField(if (prettyFieldNames) PRETTY_FIELD_NAME_CAUSE_STRING else FIELD_NAME_CAUSE_STRING, createExceptionCauseJson(error, classNameType, classNameFallback, detailsConverter, prettyFieldNames, removeCompanionObjectSuffix)),
      JField(if (prettyFieldNames) PRETTY_FIELD_NAME_SUPPRESSED_ERRORS_STRING else FIELD_NAME_SUPPRESSED_ERRORS_STRING, createExceptionSuppressedExceptionsJson(error, classNameType, classNameFallback, detailsConverter, prettyFieldNames, removeCompanionObjectSuffix))
    )
  }

  private[this] def createExceptionTypeJson(exception: Throwable,
                                            classNameType: ClassNameType,
                                            classNameFallback: Option[String],
                                            removeCompanionObjectSuffix: Boolean): JString = JString {
    EXCEPTION_FORMATTER_UTILS
      .extractClassName(exception, classNameType, classNameFallback, removeCompanionObjectSuffix)
      .getOrElse(EXCEPTION_TYPE_NOT_RETRIEVABLE)
  }

  private[this] def createExceptionMessageJson(exception: Throwable): JString = JString {
    EXCEPTION_FORMATTER_UTILS
      .retrieveMessage(exception, None, EXCEPTION_MESSAGE_FALLBACK)
  }


  private[this] def createExceptionDetailsJson(exception: Throwable,
                                               details: Option[JValue],
                                               detailConverter: Option[Throwable => JValue]): JValue = {
    Try {
      //The custom provided details will checked first.
      details match {
        //When no custom details are provided, or they are JNull or JNothing, then the provided details converter will be used.
        case None | Some(JNothing | JNull) => detailConverter match {
          //When no Details Converter is provided, then the No-Details-Converter will be used.
          case None => noDetailsConverter(exception)
          //When the No-Details Converter is provided, then it will be used and all exceptions will be handled with this one.
          case Some(dc) if dc.toString() == NO_DETAILS_CONVERTER.toString() => noDetailsConverter(exception)
          //Any other Details-Converter will be handled as follows.
          case Some(dc) => dc(exception)
        }
        //Or else the provided details will be used.
        case Some(det) => det
      }
    }.recoverWith { case e => EXCEPTION_FORMATTER_UTILS.logger.warn(ErrorDuringDetailsCreation, e); Failure(e) }
      .fold(_ => JNothing, details => details)
  }


  private[this] def createExceptionCauseJson(exception: Throwable,
                                             classNameType: ClassNameType,
                                             classNameFallback: Option[String],
                                             detailsConverter: Option[Throwable => JValue],
                                             prettyFieldNames: Boolean,
                                             removeCompanionObjectSuffix: Boolean): JValue = {
    Try {
      Option(exception)
        .flatMap(e => Option(e.getCause))
        .map(createDetailsForException(_, classNameType, classNameFallback, None, detailsConverter, prettyFieldNames, removeCompanionObjectSuffix))
    }.recoverWith { case e => EXCEPTION_FORMATTER_UTILS.logger.warn(ErrorDuringCauseDetailsCreation, e); Failure(e) }
      .fold(_ => JNothing, cause => cause.getOrElse(JNothing))
  }


  private[this] def createExceptionSuppressedExceptionsJson(exception: Throwable,
                                                            classNameType: ClassNameType,
                                                            classNameFallback: Option[String],
                                                            detailsConverter: Option[Throwable => JValue],
                                                            prettyFieldNames: Boolean,
                                                            removeCompanionObjectSuffix: Boolean): JValue = {
    Try {
      Option(exception)
        .flatMap(e => Option(e.getSuppressed).filter(_.nonEmpty))
        .map(suppressed => suppressed.map(createDetailsForException(_, classNameType, classNameFallback, None, detailsConverter, prettyFieldNames, removeCompanionObjectSuffix)))
        .map(suppressedDetails => JArray(suppressedDetails.toList))
    }.recoverWith { case e => EXCEPTION_FORMATTER_UTILS.logger.warn(ErrorDuringSuppressedExceptionsDetailsCreation, e); Failure(e) }
      .fold(_ => JNothing, suppressed => suppressed.getOrElse(JNothing))
  }


  private[this] def noDetailsConverter(error: Throwable): JValue = JNothing
}


/**
  * The Exception Json Formatter Companion Object.
  */
object ExceptionJsonFormatter {

  /**
    * Creates the Exception Json Formatter without a provided names object.
    *
    * @return    the [[ExceptionJsonFormatter]].
    */
  def apply: ExceptionJsonFormatter = new ExceptionJsonFormatter()(new Names())

  /**
    * Creates the Exception Json Formatter with a provided names object.
    *
    * @param names    the [[Names]]-object.
    *
    * @return    the [[ExceptionJsonFormatter]].
    */
  def apply(names: Names) = new ExceptionJsonFormatter()(names)

  private[ExceptionJsonFormatter] val FIELD_NAME_TYPE_STRING: String = "type"
  private[ExceptionJsonFormatter] val FIELD_NAME_MESSAGE_STRING: String = "message"
  private[ExceptionJsonFormatter] val FIELD_NAME_DETAILS_STRING: String = "details"
  private[ExceptionJsonFormatter] val FIELD_NAME_CAUSE_STRING: String = "cause"
  private[ExceptionJsonFormatter] val FIELD_NAME_SUPPRESSED_ERRORS_STRING: String = "suppressedErrors"

  private[ExceptionJsonFormatter] val PRETTY_FIELD_NAME_TYPE_STRING: String = "Type"
  private[ExceptionJsonFormatter] val PRETTY_FIELD_NAME_MESSAGE_STRING: String = "Message"
  private[ExceptionJsonFormatter] val PRETTY_FIELD_NAME_DETAILS_STRING: String = "Details"
  private[ExceptionJsonFormatter] val PRETTY_FIELD_NAME_CAUSE_STRING: String = "Cause"
  private[ExceptionJsonFormatter] val PRETTY_FIELD_NAME_SUPPRESSED_ERRORS_STRING: String = "Suppressed-Errors"

  private[ExceptionJsonFormatter] val EXCEPTION_TYPE_NOT_RETRIEVABLE: String = "-- The Exception Type was not retrievable --"
  private[ExceptionJsonFormatter] val EXCEPTION_MESSAGE_FALLBACK: String = "-"

  private[ExceptionJsonFormatter] def ErrorDuringDetailsCreation: String = "The exception details could not have been created. JNothing will be returned instead."

  private[ExceptionJsonFormatter] def ErrorDuringCauseDetailsCreation: String = "The exception details for the cause could not have been created. JNothing will be returned instead."

  private[ExceptionJsonFormatter] def ErrorDuringSuppressedExceptionsDetailsCreation: String = "The exception details for the suppressed exceptions could not have been created. JNothing will be returned instead."
}