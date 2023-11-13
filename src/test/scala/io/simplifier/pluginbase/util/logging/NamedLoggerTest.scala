package io.simplifier.pluginbase.util.logging

import io.simplifier.pluginbase.util.logging.NamedLogger.Names
import org.mockito.ArgumentCaptor
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{never, verify, when}
import org.mockito.MockitoSugar._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.slf4j.{Logger, Marker}

import scala.collection.mutable

class NamedLoggerTest extends AnyFlatSpec with Matchers {

  "A NamedLogger" should "log info if info is enabled" in new BaseFixture {
    when(logger.isInfoEnabled).thenReturn(true)
    when(logger.isInfoEnabled(marker)).thenReturn(true)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      true shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.info(msg)
    verify(logger).info(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    msgCaptor.getValue shouldBe ": test"
    namedLogger.info(marker, msg)
    verify(logger).info(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "not log info if info is disabled" in new BaseFixture {
    when(logger.isInfoEnabled).thenReturn(false)
    when(logger.isInfoEnabled(marker)).thenReturn(false)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      false shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.info(msg)
    verify(logger, never()).info(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    namedLogger.info(marker, msg)
    verify(logger, never()).info(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "log trace if trace is enabled" in new BaseFixture {
    when(logger.isTraceEnabled()).thenReturn(true)
    when(logger.isTraceEnabled(marker)).thenReturn(true)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      true shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.trace(msg)
    verify(logger).trace(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    msgCaptor.getValue shouldBe ": test"
    namedLogger.trace(marker, msg)
    verify(logger).trace(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "not log trace if trace is disabled" in new BaseFixture {
    when(logger.isTraceEnabled()).thenReturn(false)
    when(logger.isTraceEnabled(marker)).thenReturn(false)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      false shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.trace(msg)
    verify(logger, never()).info(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    namedLogger.trace(marker, msg)
    verify(logger, never()).info(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "log debug if debug is enabled" in new BaseFixture {
    when(logger.isDebugEnabled()).thenReturn(true)
    when(logger.isDebugEnabled(marker)).thenReturn(true)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      true shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.debug(msg)
    verify(logger).debug(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    msgCaptor.getValue shouldBe ": test"
    namedLogger.debug(marker, msg)
    verify(logger).debug(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "not log debug if debug is disabled" in new BaseFixture {
    when(logger.isDebugEnabled()).thenReturn(false)
    when(logger.isDebugEnabled(marker)).thenReturn(false)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      false shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.debug(msg)
    verify(logger, never()).debug(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    namedLogger.debug(marker, msg)
    verify(logger, never()).debug(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "log warn if warn is enabled" in new BaseFixture {
    when(logger.isWarnEnabled()).thenReturn(true)
    when(logger.isWarnEnabled(marker)).thenReturn(true)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      true shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.warn(msg)
    verify(logger).warn(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    msgCaptor.getValue shouldBe ": test"
    namedLogger.warn(marker, msg)
    verify(logger).warn(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "not log warn if warn is disabled" in new BaseFixture {
    when(logger.isWarnEnabled()).thenReturn(false)
    when(logger.isWarnEnabled(marker)).thenReturn(false)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      false shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.warn(msg)
    verify(logger, never()).warn(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    namedLogger.warn(marker, msg)
    verify(logger, never()).warn(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "log error if error is enabled" in new BaseFixture {
    when(logger.isErrorEnabled()).thenReturn(true)
    when(logger.isErrorEnabled(marker)).thenReturn(true)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      true shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.error(msg)
    verify(logger).error(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    msgCaptor.getValue shouldBe ": test"
    namedLogger.error(marker, msg)
    verify(logger).error(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  it should "not log error if error is disabled" in new BaseFixture {
    when(logger.isErrorEnabled()).thenReturn(false)
    when(logger.isErrorEnabled(marker)).thenReturn(false)

    implicit val names: Names = new Names()
    val namedLogger: NamedLogger = (new NamedLogger).withLogger(logger)
    lazy val msg: String = {
      false shouldBe true
      "test"
    }
    val msgCaptor: ArgumentCaptor[String] = ArgumentCaptor.forClass(classOf[String])
    val markerCaptor: ArgumentCaptor[Marker] = ArgumentCaptor.forClass(classOf[Marker])
    namedLogger.error(msg)
    verify(logger, never()).error(msgCaptor.capture(), any[mutable.WrappedArray[Object]]())
    namedLogger.error(marker, msg)
    verify(logger, never()).error(markerCaptor.capture(), msgCaptor.capture(), any[mutable.WrappedArray[Object]]())

  }

  trait BaseFixture {
    val logger: Logger = mock[Logger]
    lazy val marker: Marker = mock[Marker]
  }

}
