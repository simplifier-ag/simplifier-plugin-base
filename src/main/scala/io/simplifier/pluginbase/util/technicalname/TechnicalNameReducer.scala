package io.simplifier.pluginbase.util.technicalname

import com.google.common.base.CharMatcher
import com.google.common.base.CharMatcher.{inRange, is}
import io.simplifier.pluginbase.util.logging.ExceptionFormatting
import org.slf4j.{Logger, LoggerFactory}

import scala.util.{Failure, Try}


trait TechnicalNameReducer {

  import TechnicalNameReducer._

  /** The logger that should be provided to the trait, in order to have the respective class in the log */
  val Logger: Logger = LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))


  /**
    * Reduces the provided string to a technical name.
    *
    * @note               the technical name consists of the following characters [a-zA-Z0-9_].
    * @param string       the string to reduce to a technical name.
    *
    * @return             the respective technical name.
    */
  def reduceToTechnicalName(string: String): String = {
    reduceToTechnicalNameTry(string, addPrefix = false).get
  }


  /**
    * Reduces the provided string to a technical name.
    *
    * @note               the technical name consists of the following characters [a-zA-Z0-9_].
    * @param string       the string to reduce to a technical name.
    * @param addPrefix    the flag to determine, whether the technical name should be prefixed with an additional <b>_</b>
    *                     in cases when the name starts with a digit.
    *
    * @return             the respective technical name.
    */
  def reduceToTechnicalName(string: String,
                            addPrefix: Boolean): String = {
    reduceToTechnicalNameTry(string, addPrefix).get
  }

  /**
    * Reduces the provided string to a technical name in a Try.
    *
    * @note               the technical name consists of the following characters [a-zA-Z0-9_]
    * @param string       the string to reduce to a technical name.
    * @param addPrefix    the flag to determine, whether the technical name should be prefixed with an additional <b>_</b>
    *                     in cases when the name starts with a digit.
    *
    * @return             the respective technical name.
    */
  def reduceToTechnicalNameTry(string: String,
                               addPrefix: Boolean): Try[String] = {
    Try {
      Logger.trace(ReducingToTechnicalName(string))
      val technicalName: String = TechnicalNameReducer.matcher.retainFrom(string)
      Logger.trace(ReducedToTechnicalNameSuccessfully(string, technicalName))
      if (addPrefix) sanitizeFirstCharacter(technicalName) else technicalName
    }.recoverWith {
      case e => Logger.error(ReductionToTechnicalNameFailed(string, e)); Failure(TechnicalNameReductionError(string, e))
    }
  }


  /**
    * Adds an additional prefixes to a name, that starts with a number.
    *
    * @param name    the name to sanitize the first character of.
    *
    * @return        the name with an additional prefix <b>_</b>
    */
  private[this] def sanitizeFirstCharacter(name: String): String = {
    name match {
      case name: String if name.charAt(0).isDigit => "_" + name
      case _ => name
    }
  }


}


/**
  * Reduce String to technical name.
  * Created by Christian Simon on 22.02.17.
  */
object TechnicalNameReducer extends ExceptionFormatting {

  val matcher: CharMatcher = inRange('a', 'z') or inRange('A', 'Z') or inRange('0', '9') or is('_')

  def reduceToTechnicalName(str: String): String = {
    matcher.retainFrom(str)
  }


  case class TechnicalNameReductionError(string: String,
                                         error: Throwable)
    extends RuntimeException(ReductionToTechnicalNameFailed(string, error), error)


  private[TechnicalNameReducer] def ReducingToTechnicalName(string: String): String = {
    s"The provided string: [$string] will be reduced to a technical name."
  }


  private[TechnicalNameReducer] def ReducedToTechnicalNameSuccessfully(string: String,
                                                                       technicalName: String): String = {
    s"The provided string: [$string] was successfully reduced to the following technical name: [$technicalName]."
  }

  private[TechnicalNameReducer] def ReductionToTechnicalNameFailed(string: String,
                                                                   error: Throwable): String = {
    val rootCauseType: String = getCauseTypeSafelyWithFallback(error)
    val rootCauseMessage: String = getCauseMessageSafelyWithFallback(error)

    s"An error of the type: [$rootCauseType] with the message: [$rootCauseMessage] occurred " +
      s"during the reduction of the provided string: [$string] to a technical name."
  }


}
