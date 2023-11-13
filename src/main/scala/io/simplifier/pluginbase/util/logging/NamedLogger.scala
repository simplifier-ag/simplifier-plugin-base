package io.simplifier.pluginbase.util.logging

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import org.slf4j.{Logger, LoggerFactory, Marker}


/**
  * A logger which prints a context before each message.
  *
  * @note the context looks as follow [COMPONENT?][SUB_COMPONENT?][FUNCTION?][SUB_FUNCTION?]([CONNECTOR?][CONNECTOR_CALL?]|[CLIENT?][CLIENT_OPERATION?])?
  */
final class NamedLogger {

  private[this] var NAMES: Names = NamedLogger.DEFAULT_NAMES_OBJECT
  private[this] var LOGGER: Logger = _


  /**
    * Augments this named logger with a [[Logger]] and [[Names]].
    *
    * @param logger    the [[Logger]] reference.
    * @param names     the [[Names]] object.
    *
    * @return          the [[NamedLogger]] with a [[Logger]] and [[Names]].
    */
  def withLogger(logger: Logger)
                (implicit names: Names): NamedLogger = {
    NAMES = names
    LOGGER = logger
    this
  }

  private[this] def ->>[T](res: T, logger: NamedLogger): NamedLogger = logger

  /* Logger functions */

  def trace(msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isTraceEnabled) {
    ->>(LOGGER.trace(augmentMessage(msg), throwable), this)
  } else this

  def trace(msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isTraceEnabled) {
    ->>(LOGGER.trace(augmentMessage(msgOrFormat), args), this)
  } else this

  def trace(marker: Marker, msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isTraceEnabled) {->>(LOGGER.trace(marker, augmentMessage(msg), throwable), this)} else this
  def trace(marker: Marker, msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isTraceEnabled) {->>(LOGGER.trace(marker, augmentMessage(msgOrFormat), args), this)} else this


  def debug(msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isDebugEnabled) {->>(LOGGER.debug(augmentMessage(msg), throwable), this)} else this
  def debug(msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isDebugEnabled) {->>(LOGGER.debug(augmentMessage(msgOrFormat), args), this)} else this
  def debug(marker: Marker, msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isDebugEnabled) {->>(LOGGER.debug(marker, augmentMessage(msg), throwable), this)} else this
  def debug(marker: Marker, msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isDebugEnabled) {->>(LOGGER.debug(marker, augmentMessage(msgOrFormat), args), this)} else this


  def info(msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isInfoEnabled) {->>(LOGGER.info(augmentMessage(msg), throwable), this)} else this
  def info(msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isInfoEnabled) {->>(LOGGER.info(augmentMessage(msgOrFormat), args), this)} else this
  def info(marker: Marker, msg: => String, throwable: => Throwable): NamedLogger = if (LOGGER.isInfoEnabled) {->>(LOGGER.info(marker, augmentMessage(msg), throwable), this)} else this
  def info(marker: Marker, msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isInfoEnabled) {->>(LOGGER.info(marker, augmentMessage(msgOrFormat), args), this)} else this


  def warn(msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isWarnEnabled) {->>(LOGGER.warn(augmentMessage(msg), throwable), this)} else this
  def warn(msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isWarnEnabled) {->>(LOGGER.warn(augmentMessage(msgOrFormat), args), this)} else this
  def warn(marker: Marker, msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isWarnEnabled) {->>(LOGGER.warn(marker, augmentMessage(msg), throwable), this)} else this
  def warn(marker: Marker, msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isWarnEnabled) {->>(LOGGER.warn(marker, augmentMessage(msgOrFormat), args), this)} else this


  def error(msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isErrorEnabled) {->>(LOGGER.error(augmentMessage(msg), throwable), this)} else this
  def error(msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isErrorEnabled) {
    ->>(LOGGER.error(augmentMessage(msgOrFormat), args), this)
  } else this

  def error(marker: Marker, msg: => String, throwable: Throwable): NamedLogger = if (LOGGER.isErrorEnabled) {
    ->>(LOGGER.error(marker, augmentMessage(msg), throwable), this)
  } else this

  def error(marker: Marker, msgOrFormat: => String, args: (() => Any)*): NamedLogger = if (LOGGER.isErrorEnabled) {
    ->>(LOGGER.error(marker, augmentMessage(msgOrFormat), args), this)
  } else this


  /**
    * Augmentation function, that creates the context and augments the message with it.
    *
    * @param msg  the message to log.
    *
    * @return     the augmented message.
    */
  private[this] def augmentMessage(msg: String): String = {
    val component: String = NAMES.component.fold("")(c => Option(c).fold("")(c => s"[$c]"))
    val subComponent: String = NAMES.subComponent.fold("")(sc => Option(sc).fold("")(sc => s"[$sc]"))
    val function: String = NAMES.function.fold("")(f => Option(f).fold("")(f => s"[$f]"))
    val subFunction: String = NAMES.subFunction.fold("")(sf => Option(sf).fold("")(sf => s"[$sf]"))
    val clientName: String = NAMES.client.fold("")(c => Option(c).fold("")(c => s"[Client: [$c]]"))
    val clientOperation: String = NAMES.clientOperation.fold("")(co => Option(co).fold("")(co => s"[Client Operation: [$co]]"))
    val connectorName: String = NAMES.connector.fold("")(c => Option(c).fold("")(c => s"[Connector: [$c]]"))
    val connectorCallName: String = NAMES.connectorCall.fold("")(cc => Option(cc).fold("")(c => s"[Connector Call: [$cc]]"))
    val originString: String = s"$component$subComponent$function$subFunction$clientName$clientOperation$connectorName$connectorCallName"
    Option(originString).fold("")(_ => s"$originString: $msg")
  }
}


/** The Named Logger Companion Object */
object NamedLogger {


  /**
    * Creates a default [[NamedLogger]] instance.
    *
    * @note     the logger class is the called class.
    * @return   the default [[NamedLogger]] instance.
    */
  def apply(): NamedLogger = {
    apply(
      logger = determineLogger(),
      names = DEFAULT_NAMES_OBJECT
    )
  }


  /**
    * Creates a new [[NamedLogger]] instance.
    *
    * @param logger    the [[Logger]] reference.
    * @param names     the [[Names]] object.
    *
    * @return          the new [[NamedLogger]] instance.
    */
  def apply(logger: Logger,
            names: Names): NamedLogger = {
    new NamedLogger().withLogger(logger)(names)
  }


  /** The initial names object */
  final val DEFAULT_NAMES_OBJECT: Names = Names(None, None, None, None, None, None, None, None)


  /**
    * The names object case class.
    *
    * @param component          the component.
    * @param subComponent       the sub-component.
    * @param function           the function.
    * @param subFunction        the sub function.
    * @param client             the authentication client.
    * @param clientOperation    the authentication client operation.
    * @param connector          the connector.
    * @param connectorCall      the connector call
    */
  case class Names(component: Option[String],
                   subComponent: Option[String],
                   function: Option[String],
                   subFunction: Option[String],
                   client: Option[String],
                   clientOperation: Option[String],
                   connector: Option[String],
                   connectorCall: Option[String]) {

    def isEmpty: Boolean = component.isEmpty &&
      subComponent.isEmpty &&
      function.isEmpty &&
      subFunction.isEmpty &&
      client.isEmpty &&
      clientOperation.isEmpty &&
      connector.isEmpty &&
      connectorCall.isEmpty

    def this() = this(None, None, None, None, None, None, None, None)

  }


  private[this] def determineLogger(): Logger = {
    LoggerFactory.getLogger(
      //Using the stack trace from Throwable to have a better performance.
      new Throwable("")
        .getStackTrace
        .collectFirst {
          case element if isCalledClass(element) => determineClassName(element)
        }.getOrElse(this.getClass.getName)
    )
  }


  private[this] def determineClassName(element: StackTraceElement): String = {
    element.getClassName match {
      case className if !className.contains("$") => className
      case className =>
        className
          .reverse
          .dropWhile(_ != '$')
          .stripPrefix("$")
          .reverse
    }
  }


  private[this] def isCalledClass(element: StackTraceElement): Boolean = {
    element match {
      case null => false
      case _ if element.getClassName.endsWith("Thread") => false
      case _ if element.getClassName.stripSuffix("$").endsWith("NamedLogger") => false
      case _ => true
    }
  }
}