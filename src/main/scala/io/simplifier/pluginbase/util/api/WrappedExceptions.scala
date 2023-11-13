package io.simplifier.pluginbase.util.api

import org.json4s.JValue

import scala.language.implicitConversions

trait WrappedExceptions {

  import WrappedExceptions._

  implicit def FromWrappedException(e: WrappedException): ApiFailure = e.asApiFailure

  implicit def FromTranslatableWrappedException(e: TranslatableWrappedException): ApiFailure = e.asApiFailure

}


object WrappedExceptions {


  abstract class WrappedException(msg: String, details: JValue, cause: Throwable) extends Exception(msg, cause) {
    def asApiFailure: ApiFailure
  }

  abstract class TranslatableWrappedException(msg: String, i18nKey: String, i18nArgs: Seq[String], details: JValue, cause: Throwable) extends Exception(msg, cause) {
    def asApiFailure: ApiFailure
  }
}