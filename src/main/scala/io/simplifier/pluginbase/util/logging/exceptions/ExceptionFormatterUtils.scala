package io.simplifier.pluginbase.util.logging.exceptions

import io.simplifier.pluginbase.util.logging.NamedLogger
import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Success, Try}


/**
  * The Exception Formatter Utils.
  *
  * @param names    the [[Names]]-Object for logging purposes.
  */
protected[logging] class ExceptionFormatterUtils(implicit val names: Names) {

  import ExceptionFormatterUtils._

  private[this] var NAMES: Names = names
  private[this] val LOGGER: Logger = LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))

  protected[exceptions] def logger: NamedLogger = new NamedLogger().withLogger(LOGGER)(NAMES)

  /**
    * Sets the implicit names object for the named logger.
    *
    * @param names    the names object.
    */
  protected[logging] def setNames(names: Names): Unit = this.NAMES = names

  /**
    * Extracts the class name safely.
    *
    * @param `class`                        the provided class.
    * @param classNameType                  the type of the class name that should be rendered into the message.
    *                                       # FULL:      This format can be also used for ClassLoading. e.g. com.itizzimo.appServer.SimplifierException.
    *                                       (Inner and Static classes returns their respective delimiter)
    *                                       # CANONICAL: This format can be used for importing but not for ClassLoading e.g. com.itizzimo.appServer.SimplifierException
    *                                       (anonymous classes returns <b>null</b>.
    *                                       # SIMPLE:    Only the class name of the exception e.g. SimplifierException.
    *                                       (anonymous classes returns nothing)
    *                                       the default case is FULL.
    * @param classNameFallback              the optional name that should be returned as a class name, when the class name is null or not retrievable. The default ist [[CLASSNAME_UNAVAILABLE]].
    * @param removeCompanionObjectSuffix    the flag that determines, whether the companion object suffix <b>$</b> at the end of the classname should be removed or not.
    *
    * @return                               the class name in a Try-Monad
    */
  protected[logging] def extractClassName(`class`: Any,
                                          classNameType: ClassNameType,
                                          classNameFallback: Option[String],
                                          removeCompanionObjectSuffix: Boolean): Try[String] = {
    Option(`class`) match {
      case None => Success(classNameFallback.getOrElse(OBJECT_UNAVAILABLE))
      case Some(c) => ((Option(c.getClass), classNameType) match {
        case (None, _) => Success(Some(classNameFallback.getOrElse(CLASS_UNAVAILABLE)))
        case (Some(clazz), FULL) => getClassname(clazz)
        case (Some(clazz), CANONICAL) => getCanonicalClassname(clazz)
        case (Some(clazz), SIMPLE) => getSimpleClassname(clazz)
        case (Some(_), _) => Success(Some(classNameFallback.getOrElse(CLASSNAME_TYPE_UNAVAILABLE)))
      }).map(_.getOrElse(classNameFallback.getOrElse(CLASSNAME_UNAVAILABLE)))
        .map {
          case className if removeCompanionObjectSuffix => className.stripSuffix(COMPANION_OBJECT_SUFFIX)
          case className => className
        }
    }
  }

  /**
    * Retrieves the message.
    *
    * @note                     a fallback will be used when an error occurs, the message itself is null or empty.
    * @param error              the error to retrieve the message from.
    * @param fallback           the custom fallback to use.
    * @param defaultFallback    the default fallback to use.
    *
    * @return                   the message or the fallback string.
    */
  protected[exceptions] def retrieveMessage(error: Throwable,
                                            fallback: Option[String],
                                            defaultFallback: String): String = {
    Try {
      Option(error)
        .flatMap(e => Option(e.getMessage))
        .filter(_.trim.nonEmpty)
        .orElse(fallback)
        .getOrElse(defaultFallback)
    }
      .recoverWith { case e => logger.warn(ErrorDuringMessageRetrieval(e, fallback, defaultFallback), e); Success(fallback.getOrElse(defaultFallback)) }
      .getOrElse(defaultFallback)
  }

  private[this] def getSimpleClassname(clazz: Class[_]): Try[Option[String]] = {
    Option(clazz) match {
      case None => Success(None)
      case Some(c) => try {
        Try(Option(c.getName.split("[\\.$]").last))
      } catch {
        case e: Throwable => logger.warn(ErrorDuringClassnameExtraction(SIMPLE, Some(CANONICAL), e)); Failure(e)
      }
    }
  }

  private[this] def getCanonicalClassname(clazz: Class[_]): Try[Option[String]] = {
    Option(clazz) match {
      case None => Success(None)
      case Some(c) => try {
        Try(Option(c.getCanonicalName))
      } catch {
        case e: Throwable => logger.warn(ErrorDuringClassnameExtraction(CANONICAL, Some(FULL), e)); Failure(e)
      }
    }
  }

  private[this] def getClassname(clazz: Class[_]): Try[Option[String]] = {
    Option(clazz) match {
      case None => Success(None)
      case Some(c) => try {
        Try(Option(c.getName))
      } catch {
        case e: Throwable => logger.warn(ErrorDuringClassnameExtraction(FULL, None, e)); Failure(e)
      }
    }
  }

}

/**
  * The Exception Formatter Utils Companion Object.
  */
object ExceptionFormatterUtils {

  /**
    * Creates the Exception Formatter Utils without a provided names object.
    *
    * @return    the [[ExceptionFormatterUtils]].
    */
  def apply: ExceptionFormatterUtils = new ExceptionFormatterUtils()(new Names())

  /**
    * Creates the Exception Formatter Utils with a provided names object.
    *
    * @param names    the [[Names]]-object.
    *
    * @return    the [[ExceptionFormatterUtils]].
    */
  def apply(names: Names): ExceptionFormatterUtils = new ExceptionFormatterUtils()(names)

  protected[ExceptionFormatterUtils] val OBJECT_UNAVAILABLE = "-- The object has not been provided --"
  protected[ExceptionFormatterUtils] val CLASS_UNAVAILABLE = "-- The class of the provided object has not been provided --"
  protected[ExceptionFormatterUtils] val CLASSNAME_UNRETRIEVEABLE = "-- The class name was not retrievable due to an occurred error --"
  protected[ExceptionFormatterUtils] val CLASSNAME_UNAVAILABLE = "-- The desired class name has not been provided --"
  protected[ExceptionFormatterUtils] val CLASSNAME_TYPE_UNAVAILABLE = "-- The desired class name type has not been provided --"
  protected[ExceptionFormatterUtils] val EXCEPTION_UNAVAILABLE = "-- The exception has not been provided --"
  protected[ExceptionFormatterUtils] val MESSAGE_UNRETRIEVEABLE = "-- The message of the exception was not retrievable due to an occurred error --"
  protected[ExceptionFormatterUtils] val MESSAGE_UNAVAILABLE = "-- The message was not provided --"

  protected[ExceptionFormatterUtils] val COMPANION_OBJECT_SUFFIX: String = "$"

  protected[this] val ERROR_TYPE_UNAVAILABLE = "-- The error type could not have been retrieved --"

  /** The trait for the string representation of a class' name. */
  sealed trait ClassNameType

  /** The case object for a full string representation of a class' name. */
  case object FULL extends ClassNameType {
    override def toString: String = "Full"
  }

  /** The case object for a canonical string representation of a class' name. */
  case object CANONICAL extends ClassNameType {
    override def toString: String = "Canonical"
  }

  /** The case object for a simple string representation of a class' name. */
  case object SIMPLE extends ClassNameType {
    override def toString: String = "Simple"
  }

  private[ExceptionFormatterUtils] def ErrorDuringMessageRetrieval(error: Throwable,
                                                                   fallback: Option[String],
                                                                   defaultFallback: String): String = {
    val fallbackString: String = fallback.fold(s"Using the default fallback message: [$defaultFallback]")(f => s"Using the provided custom fallback message: [$f]")

    s"The message could not be extracted due to the error of the type: [${errorType(error)}] with the message: [${errorMessage(error)}]. $fallbackString."
  }

  private[ExceptionFormatterUtils] def ErrorDuringClassnameExtraction(name: ClassNameType,
                                                                      fallback: Option[ClassNameType],
                                                                      error: Throwable): String = {
    val fallbackString: String = fallback.fold("No fallback is possible anymore")(f => s"Falling back to a $f representation")

    s"The class name with a $name representation could not be extracted due to the error of the type: [${errorType(error)}] " +
      s"with the message: [${errorMessage(error)}]. $fallbackString."
  }

  private[this] def errorType(error: Throwable): String = Option(error) match {
    case None => EXCEPTION_UNAVAILABLE
    case Some(e) => Try(e.getClass.getName).getOrElse(ERROR_TYPE_UNAVAILABLE)
  }

  private[this] def errorMessage(error: Throwable): String = Option(error) match {
    case None => EXCEPTION_UNAVAILABLE
    case Some(e) if Try(e.getMessage).isFailure => MESSAGE_UNRETRIEVEABLE
    case Some(e) if e.getMessage.trim.isEmpty => MESSAGE_UNAVAILABLE
    case Some(e) => e.getMessage
  }
}