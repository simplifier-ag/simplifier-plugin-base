package io.simplifier.pluginbase.util.config

import com.typesafe.config.Config

import java.util.concurrent.TimeUnit
import scala.collection.JavaConverters._
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.Try
import scala.util.control.NonFatal
import scala.util.matching.Regex
import io.simplifier.pluginbase.util.logging.Logging


/**
  * Scala-friendly [[Config]] extension to support reading of config values as [[Option]] or with a default value if the path doesn't exist.
  * To use the extensions, simply import `io.simplifier.pluginbase.util.ConfigExtension._` to your scope.
  * Created by Christian Simon on 01.09.16.
  */
object ConfigExtension extends Logging {

  class AugmentedConfig(config: Config) {

    def get[A: TypeTag : ClassTag](path: String): A = {
      if (typeOf[A] =:= typeOf[String]) config.getString(path).asInstanceOf[A]
      else if (typeOf[A] =:= typeOf[Int]) config.getInt(path).asInstanceOf[A]
      else if (typeOf[A] =:= typeOf[Boolean]) config.getBoolean(path).asInstanceOf[A]
      else if (typeOf[A] =:= typeOf[Double]) config.getDouble(path).asInstanceOf[A]
      else if (typeOf[A] =:= typeOf[Long]) config.getLong(path).asInstanceOf[A]
      else if (typeOf[A] =:= typeOf[FiniteDuration]) config.getConfig(path).asInstanceOf[A]
      else if (typeOf[A] =:= typeOf[Config]) config.getConfig(path).asInstanceOf[A]
      else config.getAnyRef(path).asInstanceOf[A]
    }

    def getOpt[A: TypeTag : ClassTag](path: String): Option[A] = getOptImpl(path) {
      get[A](path)
    }

    def get[A: TypeTag : ClassTag](path: String, defaultValue: => A): A = {
      getOpt[A](path).getOrElse(defaultValue)
    }

    def getConfigOpt(path: String): Option[Config] = getOptImpl(path) {
      config.getConfig(path)
    }

    def getBytesOpt(path: String): Option[Long] = getOptImpl(path) {
      config.getBytes(path)
    }

    def getBytes(path: String, defaultValue: => Long): Long = getBytesOpt(path).getOrElse(defaultValue)


    def getStringOpt(path: String): Option[String] = getOptImpl(path) {
      config.getString(path)
    }

    def getString(path: String, defaultValue: => String): String = getStringOpt(path).getOrElse(defaultValue)

    def getStringListOpt(path: String): Option[Seq[String]] = getOptImpl(path) {
      config.getStringList(path).asScala
    }

    def getList[T](path: String, defaultValue: => Seq[T]): Seq[T] = getListOpt[T](path).getOrElse(defaultValue)

    def getListOpt[T](path: String): Option[Seq[T]] = getOptImpl(path) {
      config.getList(path).asScala.flatMap { value =>
        Try(value.unwrapped().asInstanceOf[T]).fold(_ => {
          logger.warn(s"Unexpected element: {$value} of type: {${value.getClass.getName}} in path: {$path} provided.")
          None
        }, ele => Some(ele))
      }
    }


    def getStringList(path: String, defaultValue: => Seq[String]): Seq[String] = getStringListOpt(path).getOrElse(defaultValue)


    def getLongOpt(path: String): Option[Long] = getOptImpl(path) {
      config.getLong(path)
    }

    def getLong(path: String, defaultValue: => Long): Long = getLongOpt(path).getOrElse(defaultValue)

    def getIntOpt(path: String): Option[Int] = getOptImpl(path) {
      config.getInt(path)
    }

    def getInt(path: String, defaultValue: => Int): Int = getIntOpt(path).getOrElse(defaultValue)

    def getBooleanOpt(path: String): Option[Boolean] = getOptImpl(path) {
      config.getBoolean(path)
    }

    def getBoolean(path: String, defaultValue: => Boolean): Boolean = getBooleanOpt(path).getOrElse(defaultValue)

    def getDurationOpt(path: String): Option[FiniteDuration] = getOptImpl(path) {
      FiniteDuration(config.getDuration(path).toMillis, TimeUnit.MILLISECONDS)
    }

    def getDuration(path: String, defaultValue: => FiniteDuration): FiniteDuration = getDurationOpt(path).getOrElse(defaultValue)


    def getDurationValueOpt(path: String): Option[Long] = {
      getOptImpl(path) {
        val durationString: String = config.getString(path)

        durationString match {
          case "" => None
          case DURATION_VALUE_REGEX(value) => Try(java.lang.Long.parseLong(value))
            .map(Some(_))
            .getOrElse {
              logger.warn(CannotParseDurationValue(durationString))
              None
            }
          case _ => logger.warn(CannotParseDurationValue(durationString)); None
        }
      }.flatten
    }

    def getDurationValue(path: String, defaultValue: => Long): Long = getDurationValueOpt(path).getOrElse(defaultValue)


    def getDurationTimeUnitOpt(path: String): Option[TimeUnit] = {
      getOptImpl(path) {
        val durationString: String = config.getString(path)

        Option(durationString match {
          case "" => TimeUnit.MILLISECONDS
          case DURATION_TIME_UNIT_REGEX(unit) if Seq("ns", "nanos", "nanoseconds").contains(unit) => TimeUnit.NANOSECONDS
          case DURATION_TIME_UNIT_REGEX(unit) if Seq("us", "micros", "microseconds").contains(unit) => TimeUnit.MICROSECONDS
          case DURATION_TIME_UNIT_REGEX(unit) if Seq(null, "ms", "millis", "milliseconds").contains(unit) => TimeUnit.MILLISECONDS
          case DURATION_TIME_UNIT_REGEX(unit) if Seq("s", "seconds").contains(unit) => TimeUnit.SECONDS
          case DURATION_TIME_UNIT_REGEX(unit) if Seq("m", "minutes").contains(unit) => TimeUnit.MINUTES
          case DURATION_TIME_UNIT_REGEX(unit) if Seq("h", "hours").contains(unit) => TimeUnit.HOURS
          case DURATION_TIME_UNIT_REGEX(unit) if Seq("d", "days").contains(unit) => TimeUnit.DAYS
          case _ => logger.warn(CannotParseDurationTimeUnit(durationString)); null
        })
      }.flatten
    }

    def getDurationTimeUnit(path: String, defaultValue: => TimeUnit): TimeUnit = getDurationTimeUnitOpt(path).getOrElse(defaultValue)


    @inline
    private[this] def getOptImpl[A](path: String)(block: => A): Option[A] = try {
      if (config.hasPath(path)) Some(block) else None
    } catch {
      case NonFatal(e) =>
        logger.warn(s"Error reading config '$path': ${e.getMessage}")
        None
    }

  }

  /**
    * Implicitly augment config.
    *
    * @param config typesafe config
    *
    * @return augmented config
    */
  implicit def augmentConfig(config: Config): AugmentedConfig = new AugmentedConfig(config)


  private val DURATION_TIME_UNIT_REGEX: Regex = """\d*\s*(\w+)""".r.unanchored
  private val DURATION_VALUE_REGEX: Regex = """(\d+)\s*\w*""".r.unanchored

  private def CannotParseDurationValue(durationString:String) = s"Cannot parse the duration value from the provided duration: [$durationString]. Returning None."
  private def CannotParseDurationTimeUnit(durationString:String) = s"Cannot parse the time unit from the provided duration: [$durationString]. Returning None."


}
