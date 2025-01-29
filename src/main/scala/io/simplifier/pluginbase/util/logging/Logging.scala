package io.simplifier.pluginbase.util.logging

import com.google.common.base.CharMatcher.{inRange, is}
import io.simplifier.pluginbase.util.logging.Logging.{CANONICAL, FULL, SIMPLE}
import io.simplifier.pluginbase.util.technicalname.TechnicalNameReducer
import org.slf4j.{Logger, LoggerFactory}

import scala.util.Try

/**
 * Trait which provides logger for Console/Logback.
 *
 * @author Christian Simon
 */
trait Logging extends LoggingBase {
  val logger: Logger = LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))
}

/**
 * Trait which provides logger for Console/Logback as Lazy Val (so the logger doesn't get initialized immediately).
 *
 * @author Christian Simon
 */
trait DeferredLogging extends LoggingBase {
  lazy val logger: Logger = LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))
}

private[logging] trait LoggingBase {

  def logger: Logger

  /**
   * Returns safely the type of the exception.
   *
   * @note any occurring Non-Fatal errors that might occur during the extraction process are ignored and will not be propagated, as this method is safe.
   *       also if the parameters are <b>null</b> fallback strings will be used.
   * @param throwable     the exception itself.
   * @param classNameType the type of the classname that should be rendered into the message.
   * @return the exception type as a string.
   */
  def getExceptionTypeSafely(throwable: Throwable, classNameType: Logging.exceptionClassName = Logging.FULL): String = {
    Option(throwable)
      .fold(Logging.EXCEPTION_UNAVAILABLE)(t => Option(extractClassName(t, classNameType)
        .getOrElse(Logging.CLASSNAME_UNRETRIEVEABLE_EXCEPTION))
        .fold(Logging.CLASSNAME_UNAVAILABLE_EXCEPTION)(name => name))
  }

  /**
   * Returns safely the classname.
   *
   * @note any occurring Non-Fatal errors that might occur during the extraction process are ignored and will not be propagated, as this method is safe.
   *       also if the parameters are <b>null</b> fallback strings will be used.
   * @param `class`       the class itself.
   * @param classNameType the type of the classname that should be rendered into the message.
   * @return the desired class name as a string.
   */
  def getClassNameSafely(`class`: Any, classNameType: Logging.exceptionClassName = Logging.FULL): String = {
    Option(`class`)
      .fold(Logging.CLASS_UNAVAILABLE)(c => Option(extractClassName(c, classNameType)
        .getOrElse(Logging.CLASSNAME_UNRETRIEVEABLE))
        .fold(Logging.CLASSNAME_UNAVAILABLE)(name => name))
  }

  /**
   * Extracts the class name safely.
   *
   * @param `class`       the class name.
   * @param classNameType the type of the classname that should be rendered into the message.
   *                      # FULL:      This format can be also used for ClassLoading. e.g. com.itizzimo.appServer.SimplifierException.
   *                      (Inner and Static classes returns their respective delimiter)
   *                      # CANONICAL: This format can be used for importing but not for ClassLoading e.g. com.itizzimo.appServer.SimplifierException
   *                      (anonymous classes returns <b>null</b>.
   *                      # SIMPLE:    Only the class name of the exception e.g. SimplifierException.
   *                      (anonymous classes returns nothing)
   *                      the default case is FULL.
   * @return the class name in a Try-Monad
   */
  private[this] def extractClassName(`class`: Any, classNameType: Logging.exceptionClassName): Try[String] = Try {
    val clazz: Class[_] = `class`.getClass
    classNameType match {
      case FULL => clazz.getName
      case CANONICAL => try {
        clazz.getCanonicalName
      } catch {
        case e: Throwable => logger.warn(Logging.ErrorDuringNameExtraction(CANONICAL, FULL, e))
          clazz.getName
      }
      case SIMPLE => try {
        clazz.getSimpleName
      } catch {
        case e: Throwable => logger.warn(Logging.ErrorDuringNameExtraction(SIMPLE, CANONICAL, e))
          try {
            clazz.getCanonicalName
          } catch {
            case e: Throwable => logger.warn(Logging.ErrorDuringNameExtraction(CANONICAL, FULL, e))
              clazz.getName
          }
      }
      case _ => Logging.CLASSNAME_TYPE_UNAVAILABLE
    }
  }
}


object Logging {

  protected[logging] val CLASS_UNAVAILABLE = "-- The class has not been provided --"
  protected[logging] val EXCEPTION_UNAVAILABLE = "-- The exception has not been provided --"
  protected[logging] val CLASSNAME_TYPE_UNAVAILABLE = "-- The desired class name type has not been provided --"
  protected[logging] val CLASSNAME_UNRETRIEVEABLE = "-- Class name is not retrievable due to an exception --"
  protected[logging] val CLASSNAME_UNRETRIEVEABLE_EXCEPTION = "-- Class name of exception is not retrievable due to an exception --"
  protected[logging] val CLASSNAME_UNAVAILABLE = "-- Class name is not available --"
  protected[logging] val CLASSNAME_UNAVAILABLE_EXCEPTION = "-- Class name of exception is not available --"
  protected[logging] val MESSAGE_UNRETRIEVEABLE = "-- Message of exception ist not retrievable due to an exception --"
  protected[logging] val MESSAGE_UNAVAILABLE = "-- Message is not available --"

  protected[logging] def ErrorDuringNameExtraction(name: exceptionClassName, fallback: exceptionClassName, error: Throwable): String = {
    s"The classname with a ${name.toString} representation could not be extracted due to the error: {${
      Option(error).fold(Logging.EXCEPTION_UNAVAILABLE)(t =>
        Option(Try(t.getMessage).getOrElse(Logging.MESSAGE_UNRETRIEVEABLE).trim).filterNot(_.isEmpty).fold(Logging.MESSAGE_UNAVAILABLE)(message => message))
    }}." +
      s"Falling back to a ${fallback.toString} representation."
  }

  /** The trait for the string representation of a exception class' name. */
  sealed trait exceptionClassName

  /** The case object for a full string representation of a exception class' name. */
  case object FULL extends exceptionClassName

  /** The case object for a canonical string representation of a exception class' name. */
  case object CANONICAL extends exceptionClassName

  /** The case object for a simple string representation of a exception class' name. */
  case object SIMPLE extends exceptionClassName

  private val ENV_CLUSTER_MEMBER_NAME = "CLUSTER_MEMBER_NAME"
  private val LOG_PROPERTY_LOGFILE_BASENAME = "LOGFILE_BASENAME"

  /**
   * Provide a system property 'LOGFILE_BASENAME' to be referenced by the internal logback.xml configuration file.
   *
   * It defines the basename part of a logfile for the current plugin. In case of cluster mode, a system property
   * or environment variable named 'CLUSTER_MEMBER_NAME' may define a postfix to make the filename unique
   * inside the shared log directory. <br/>
   * <b>This function should be called before the very first log output, so even before settings.conf parsing.</b>
   *
   * @param defaultBaseName Normal base name of the logfile (e.g, "simplifier" for "simplifier.log")
   */
  def initializeLoggingProperties(defaultBaseName: String): Unit = {
    val logfileBasename = Option(System.getProperty(ENV_CLUSTER_MEMBER_NAME))
        .orElse(Option(System.getenv(ENV_CLUSTER_MEMBER_NAME)))
        .map((inRange('a', 'z') or inRange('A', 'Z') or inRange('0', '9') or is('_')).retainFrom).filter(_.nonEmpty)
        .map(clusterMemberName => s"$defaultBaseName-$clusterMemberName")
        .getOrElse(defaultBaseName)

    System.setProperty(LOG_PROPERTY_LOGFILE_BASENAME, logfileBasename)
  }

}
