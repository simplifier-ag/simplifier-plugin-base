package io.simplifier.pluginbase.security

import akka.http.scaladsl.model.StatusCodes.Forbidden
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directive0
import akka.http.scaladsl.server.Directives.{complete, failWith, pass}
import io.simplifier.pluginapi.rest.PluginHeaders._
import io.simplifier.pluginbase.PluginSettings
import io.simplifier.pluginbase.SimplifierPlugin.AppServerInformation
import io.simplifier.pluginbase.util.logging.Logging

import scala.annotation.tailrec
import scala.util.Try
import scala.util.matching.Regex


/**
  * The trusted source validator, that verifies a request source against a provided blacklist OR whitelist.
  *
  * @param pluginSettings        the plugin settings, where the security configurations are stored.
  * @param appServerInformation  the AppServer information, where the prefix is stored.
  */
class TrustedSourcesValidator(pluginSettings: PluginSettings, appServerInformation: AppServerInformation) extends Logging {

  import io.simplifier.pluginbase.security.TrustedSourcesValidator._

  /**
    * Directive to verify whether the provided request source is blacklisted or whitelisted, according to the provided lists.
    *
    * @param requestSource  the request source to verify.
    *
    * @return               <b>pass</b> when the request source is whitelisted or not in the provided blacklist, <b>Forbidden</b> when the request source is
    *                       not whitelisted or blacklisted, <b>failWith</b> when the provided configuration is faulty.
    */
  def verifyOriginalRequestSource(requestSource: RequestSource): Directive0 = {

    val sanitizedAllowUntrustedSources: Boolean = Option(pluginSettings)
      .flatMap(ps => Option(ps.security))
      .flatMap(sec => Option(sec.IncomingRequests))
      .flatMap(ir => Option(ir.allowUntrustedSources))
      .fold {
        logger.warn("Allow untrusted sources flag was null. Falling back to default: {true}")
        true
      }(auts => auts)


    if (sanitizedAllowUntrustedSources) pass else {
      val sanitizedBlacklist: Seq[String] = Option(pluginSettings)
        .flatMap(ps => Option(ps.security))
        .flatMap(sec => Option(sec.IncomingRequests))
        .flatMap(ir => Option(ir.trustedSources))
        .flatMap(trs => Option(trs.blackList))
        .flatMap(bl => Option(bl))
        .fold {
          logger.warn("Provided blacklist was null. Falling back to default: []")
          Seq.empty[String]
        }(bl => bl)
        .filterNot(_ == null)
        .filter(_.trim.nonEmpty)

      val sanitizedWhitelist: Seq[String] = Option(pluginSettings)
        .flatMap(ps => Option(ps.security))
        .flatMap(sec => Option(sec.IncomingRequests))
        .flatMap(ir => Option(ir.trustedSources))
        .flatMap(trs => Option(trs.whiteList))
        .flatMap(wl => Option(wl))
        .fold {
          logger.warn("Provided whitelist was null. Falling back to default: []")
          Seq.empty[String]
        }(wl => wl)
        .filterNot(_ == null)
        .filter(_.trim.nonEmpty)

      val sanitizedAppServerPrefix: Uri = Option(appServerInformation)
        .flatMap(appSi => Option(appSi.prefix))
        .flatMap(pref => pref.flatMap(Option(_)))
        .fold(DEFAULT_APP_SERVER_PREFIX)(pref => Try(Uri(pref)).fold(e => {
          logger.warn(s"Cannot create Uri from: {$pref} due to the error of the type: {${e.getClass.getName}}, The reason is: {${e.getMessage}}")
          DEFAULT_APP_SERVER_PREFIX
        }, u => u))


      val uri: Uri = (requestSource match {
        case null | AppServer(null) | AppServerDirect(null) | AppServerDirect(Some(null)) | BusinessObject(_, null) | Plugin(_, null) | Plugin(_, Some(null)) => None
        case AppServer(initialSource) => initialSource
        case AppServerDirect(initialSource) => initialSource
        case BusinessObject(_, initialSource) => initialSource
        case Plugin(_, appServerSource) => appServerSource
      }).getOrElse(Uri())

      if (uri == sanitizedAppServerPrefix) pass else {
        (sanitizedBlacklist.nonEmpty, sanitizedWhitelist.nonEmpty) match {
          case (true, true) if isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.whiteList) &&
            isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.blackList) => complete(Forbidden -> s"The source: {${uri.toString()}} albeit being in the whitelist is blacklisted.")
          case (true, true) if isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.whiteList) &&
            !isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.blackList) => pass
          case (true, false) if isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.blackList) => complete(Forbidden -> s"The source: {${uri.toString()}} is blacklisted.")
          case (true, false) if !isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.blackList) => pass
          case (false, true) if isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.whiteList) => pass
          case (false, true) | (true, true) if !isListed(uri, pluginSettings.security.IncomingRequests.trustedSources.whiteList) => complete(Forbidden -> s"The source: {${uri.toString()}} is not part of the provided white list.")
          case (false, false) => failWith(new RuntimeException("When the source verification is activated, then a blacklist or whitelist must be provided."))
        }
      }
    }
  }


  /**
    * Verifies whether the provided source is listed in the provided trusted sources.
    *
    * @param providedUri     the provided source to verify.
    * @param trustedSources  the trusted sources.
    *
    * @return                <b>true</b> when the provided source is listed in the trusted sources <b>false</b> otherwise
    */
  private[security] def isListed(providedUri: Uri, trustedSources: Seq[String]): Boolean = {

    /**
      * Matches a source against a provided regex.
      *
      * @param source   the source to match.
      * @param regex    the regex to match against.
      *
      * @return         <b>true</b> if the source matches the regex, <b>false</b> otherwise.
      */
    def matchesRegex(source: String, regex: Regex): Boolean = source match {
      case regex(_*) => true
      case _ => false
    }


    val sanitizedSources: Seq[String] = Option(trustedSources).map(seq => seq.filter(_ != null).filter(_.trim.nonEmpty)).getOrElse(Seq())
    if (providedUri == null || providedUri.isEmpty) false else {
      providedUri.toString() match {
        case IPV4_ADDRESS(_*) => sanitizedSources.filter(matchesRegex(_, IPV4_ADDRESS)).exists(matchesTrust(_, providedUri, IPv4Warning))
        case IPV6_ADDRESS(_*) => false
        case WILDCARD_DOMAIN(_*) => sanitizedSources.filter(matchesRegex(_, WILDCARD_DOMAIN)).exists(matchesTrust(_, providedUri, DomainWarning))
      }
    }
  }


  /**
    * Validates whether the provided Uri is a trusted domain/IPv4 address.
    *
    * @param trustedSource       the trusted source domain Uri to validate against.
    * @param providedSource      the provided Uri to validate.
    *
    * @return                    <b>true</b> when the provided Uri is trusted <b>false</b> otherwise.
    */
  private[security] def matchesTrust(trustedSource: String, providedSource: Uri, fallbackLog: (Throwable, Uri, String) => String): Boolean = Try {
    if (Option(trustedSource).isEmpty || Option(providedSource).isEmpty) false else if (providedSource.isEmpty) false else {
      val schemeSource: Option[String] = extractScheme(Left(trustedSource))
      val schemeProvided: Option[String] = extractScheme(Right(providedSource))

      val portScheme: Option[String] = extractPort(Left(trustedSource))
      val portProvided: Option[String] = extractPort(Right(providedSource))

      val partsScheme: Seq[String] = splitUri(Left(trustedSource))
      val partsProvided: Seq[String] = splitUri(Right(providedSource))

      val schemeTrusted: Boolean = schemeSource.fold(true)(s => if (s == WILDCARD || trustedSource.trim == WILDCARD) true else schemeProvided.fold(false)(ss => ss.trim == s.trim))
      val partsTrusted: Boolean = validateParts(partsProvided, partsScheme)
      val portTrusted: Boolean = portScheme.fold(true)(p => if (p == WILDCARD || trustedSource.trim == WILDCARD) true
      else portProvided.fold(false)(ps => Integer.parseInt(ps) == Integer.parseInt(p)))

      schemeTrusted && partsTrusted && portTrusted
    }
  }.fold(e => {
    logger.warn(fallbackLog(e, providedSource, trustedSource))
    false
  }, res => res)


  /**
    * Validates the provided parts against the trusted parts.
    *
    * @param providedParts   the provided parts to validate.
    * @param trustedParts    the trusted parts to validate against.
    *
    * @return                <b>true</b> when the provided parts matches the trusted parts <b>false</b> otherwise.
    */
  @tailrec
  final private[security] def validateParts(providedParts: Seq[String], trustedParts: Seq[String]): Boolean = {

    /**
      * Validate the provided part against the trusted part.
      *
      * @param providedPart  the provided part to validate.
      * @param trustedPart   the trusted part to validate against.
      *
      * @return              <b>true</b> when the provided part match the trusted part <b>false</b> otherwise.
      */
    def validatePart(providedPart: String, trustedPart: String): Boolean = if (trustedPart == "*") true else providedPart == trustedPart

    if (Option(providedParts).isEmpty || Option(trustedParts).isEmpty) false
    else if (providedParts.contains(null) || trustedParts.contains(null)) false
    else if (providedParts.isEmpty || trustedParts.isEmpty) false
    else if (trustedParts.lengthCompare(providedParts.length) > 0) false
    else {
      (providedParts.headOption, trustedParts.headOption) match {
        case (None, None) => true
        case (None, Some(_)) if trustedParts.lengthCompare(1) >= 0 && !isOnlyWildcard(trustedParts) => false
        case (None, Some(_)) if trustedParts.lengthCompare(1) >= 0 && isOnlyWildcard(trustedParts) => true
        case (Some(_), None) => false
        case (Some(_), Some(_)) if trustedParts.lengthCompare(1) == 0 && isOnlyWildcard(trustedParts) => true
        case (Some(dp), Some(tp)) if validatePart(dp, tp) && (providedParts.drop(1).nonEmpty || trustedParts.drop(1).nonEmpty) =>
          validateParts(providedParts.drop(1), trustedParts.drop(1))
        case (Some(dp), Some(tp)) if validatePart(dp, tp) => true
        case (Some(_), Some(_)) => false
      }
    }
  }


  /**
    * Checks whether the provided parts of a trusted source only consists of wildcards.
    *
    * @param trustedParts  the trusted parts.
    *
    * @return              <b>true</b> when the provided parts only consists of wildcard <b>false</b> otherwise.
    */
  private[security] def isOnlyWildcard(trustedParts: Seq[String]): Boolean = Option(trustedParts).fold(false)(_.forall(_ == "*"))


  /**
    * Sanitizes a provided Uri.
    *
    * @param uri  the Uri either as a String or as an Uri-Object
    *
    * @return     the sanitized Uri.
    */
  private[security] def sanitizeUri(uri: Either[String, Uri]): String = {
    Option(uri).fold(REPLACEMENT) {
      case Left(null) => REPLACEMENT
      case Left(us) => us.trim
      case Right(null) => REPLACEMENT
      case Right(uu) => uu.toString.trim.stripPrefix(SCHEME_REMNANTS)
    }
  }

  /**
    * Extracts the scheme from a provided Uri.
    *
    * @param uri the Uri.
    *
    * @return    the extracted scheme.
    */
  private[security] def extractScheme(uri: Either[String, Uri]): Option[String] = {
    Option(uri).flatMap { u =>
      val sanitizedUri: String = sanitizeUri(u)
      if (sanitizedUri.contains(SCHEME_SEPARATOR)) sanitizedUri.split(SCHEME_SEPARATOR).headOption.map(_.toLowerCase) else None
    }.map(_.trim)
  }

  /**
    * Extracts the port from a provided Uri.
    *
    * @param uri      the Uri.
    *
    * @return         the extracted port.
    */
  private[security] def extractPort(uri: Either[String, Uri]): Option[String] = {
    Option(uri).flatMap { u =>
      sanitizeUri(u) match {
        case su if su.replace(s"$SCHEME_SEPARATOR", REPLACEMENT).contains(PORT_SEPARATOR) =>
          su.replace(s"$SCHEME_SEPARATOR", REPLACEMENT).split(PORT_SEPARATOR).lastOption
        case _ => None
      }
    }.map(_.trim)
  }

  /**
    * Splits the Uri into separate parts denominated by an <b>.</b>.
    *
    * @param uri      the Uri.
    *
    * @return         the split parts of an Uri.
    */
  private[security] def splitUri(uri: Either[String, Uri]): Seq[String] = {
    Option(uri).fold(Seq.empty[String]) { u =>
      val sanitizedUri: String = sanitizeUri(u)
      (sanitizedUri.split(PART_SEPARATOR).length, extractScheme(Left(sanitizedUri)), extractPort(Left(sanitizedUri))) match {
        case (0, None, None) => Seq(sanitizedUri)
        case (0, None, Some(p)) => Seq(sanitizedUri.replace(s"$PORT_SEPARATOR$p", REPLACEMENT))
        case (0, Some(s), None) => Seq(sanitizedUri.replace(s"$s$SCHEME_SEPARATOR", REPLACEMENT))
        case (0, Some(s), Some(p)) => Seq(sanitizedUri.replace(s"$s$SCHEME_SEPARATOR", REPLACEMENT).replace(s"$PORT_SEPARATOR$p", REPLACEMENT))
        case (_, None, None) => sanitizedUri.split(PART_SEPARATOR)
        case (_, None, Some(p)) => sanitizedUri.replace(s"$PORT_SEPARATOR$p", REPLACEMENT).split(PART_SEPARATOR)
        case (_, Some(s), None) => sanitizedUri.replace(s"$s$SCHEME_SEPARATOR", REPLACEMENT).split(PART_SEPARATOR)
        case (_, Some(s), Some(p)) => sanitizedUri.replace(s"$s$SCHEME_SEPARATOR", REPLACEMENT).replace(s"$PORT_SEPARATOR$p", REPLACEMENT).split(PART_SEPARATOR)
      }
    }.filterNot(_ == REPLACEMENT)
  }
}


/**
  * The trusted validator companion object.
  */
object TrustedSourcesValidator {
  private[security] def DomainWarning(error: Throwable, source: Uri, trusted: String): String =
    s"An error: {${error.getClass.getName}} occurred during the domain validation process of the source: {${source.toString}} " +
      s"against the trusted source: {$trusted}. The reason was: {${error.getMessage}}"

  private[security] def IPv4Warning(error: Throwable, source: Uri, trusted: String): String =
    s"An error: {${error.getClass.getName}} occurred during the IPv4-Address validation process of the source: {${source.toString}} " +
      s"against the trusted source: {$trusted}. The reason was: {${error.getMessage}}"


  private[TrustedSourcesValidator] val WILDCARD_DOMAIN: Regex =
    ("""^(?:((?:(?:[H|h][T|t][T|t][P|p][S|s]?)|[W|w][S|s][S|s]?)|\*)://)?((?:[A-Za-z0-9]+(?:-[A-Za-z0-9]+)*\.|\*(?:\.\*)*\.)+[A-Za-z]{2,}|""" +
        """(?:[A-Za-z0-9]+(?:-[A-Za-z0-9]+)*\.|\*(?:\.\*)*\.)*(?:[A-Za-z0-9]+|\*)|""" +
        """(?:[A-Za-z0-9]*(?:\-?[A-Za-z0-9]*)*|\*(?:\.\*)*))\.?(:(?:[0-9]{1,5}|\*))?$""").r
  private[TrustedSourcesValidator] val IPV4_ADDRESS: Regex =
    ("^(?:(?:(?:(?:[H|h][T|t][T|t][P|p][S|s]?)|[W|w][S|s][S|s]?)|\\*)://)?(?:(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|\\*)\\.){3}" +
      "(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|\\*)|\\*)(?::(?:(?:[0-9]{1,5})|\\*))?$").r
  private[TrustedSourcesValidator] val IPV6_ADDRESS: Regex =
    ("^(?:(?:(?:[A-F0-9]{1,4}:){6}|(?=(?:[A-F0-9]{0,4}:){0,6}(?:[0-9]{1,3}\\.){3}[0-9]{1,3}$)(([0-9A-F]{1,4}:){0,5}|:)((:[0-9A-F]{1,4}){1,5}:|:)|" +
      "::(?:[A-F0-9]{1,4}:){5})(?:(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])\\.){3}(?:25[0-5]|2[0-4][0-9]|1[0-9][0-9]|[1-9]?[0-9])|" +
      "(?:[A-F0-9]{1,4}:){7}[A-F0-9]{1,4}|(?=(?:[A-F0-9]{0,4}:){0,7}[A-F0-9]{0,4}$)(([0-9A-F]{1,4}:){1,7}|:)((:[0-9A-F]{1,4}){1,7}|:)|" +
      "(?:[A-F0-9]{1,4}:){7}:|:(:[A-F0-9]{1,4}){7})$").r


  private[TrustedSourcesValidator] val SCHEME_SEPARATOR: String = "://"
  private[TrustedSourcesValidator] val SCHEME_REMNANTS: String = "//"
  private[TrustedSourcesValidator] val PORT_SEPARATOR: String = ":"
  private[TrustedSourcesValidator] val PART_SEPARATOR: String = "\\."
  private[TrustedSourcesValidator] val REPLACEMENT: String = ""
  private[TrustedSourcesValidator] val WILDCARD: String = "*"
  private[TrustedSourcesValidator] val DEFAULT_APP_SERVER_PREFIX: Uri = Uri("http://localhost:8080")

}
