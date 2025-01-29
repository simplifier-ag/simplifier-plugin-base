package io.simplifier.pluginbase.util.logging

import org.scalatest.Ignore
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers


@Ignore("Ignored for automated build")
class LoggingTest extends AnyFlatSpecLike with Matchers {

  behavior of "Logging.initializeLoggingProperties"

  it should "use postfix from system variable if system property is not set" in new Fixture {
    setVariable(ENV_CLUSTER_MEMBER_NAME, "VARIABLE_VALUE")
    Logging.initializeLoggingProperties(defaultName)
    System.getProperty(LOG_PROPERTY_LOGFILE_BASENAME) should be (s"$defaultName-VARIABLE_VALUE")
  }

  it should "preferably use postfix from system property" in new Fixture {
    setProperty(ENV_CLUSTER_MEMBER_NAME, "PROPERTY_VALUE")
    setVariable(ENV_CLUSTER_MEMBER_NAME, "something else")
    Logging.initializeLoggingProperties(defaultName)
    System.getProperty(LOG_PROPERTY_LOGFILE_BASENAME) should be (s"$defaultName-PROPERTY_VALUE")
  }

  it should "filter postfix to contain technical name characters only" in new Fixture {
    setProperty(ENV_CLUSTER_MEMBER_NAME, "some-weird-name-with-special-chars-!@#$%^&*()_+=-`~[]{}|;':\",.<>/?")
    Logging.initializeLoggingProperties(defaultName)
    System.getProperty(LOG_PROPERTY_LOGFILE_BASENAME) should be (s"$defaultName-someweirdnamewithspecialchars_")
  }

  it should "ignore postfix if it contains no technical name characters" in new Fixture {
    setProperty(ENV_CLUSTER_MEMBER_NAME, "!@#$%^&*()+=-`~[]{}|;':\",.<>/?")
    Logging.initializeLoggingProperties(defaultName)
    System.getProperty(LOG_PROPERTY_LOGFILE_BASENAME) should be (defaultName)
  }

  it should "keep default name when neither env var or system property is set" in new Fixture {
    Logging.initializeLoggingProperties(defaultName)
    System.getProperty(LOG_PROPERTY_LOGFILE_BASENAME) should be (defaultName)
  }



  trait Fixture {
    val defaultName = "default"
    val LOG_PROPERTY_LOGFILE_BASENAME = "LOGFILE_BASENAME"
    val ENV_CLUSTER_MEMBER_NAME = "CLUSTER_MEMBER_NAME"

    def setProperty(name: String, value: String): Unit = System.setProperty(name, value)
    def clearSystemProperty(name: String): Unit = System.clearProperty(name)

    def setVariable(name: String, value: String): String = {
      val env: java.util.Map[String, String] = System.getenv()
      val cl = env.getClass
      val field = cl.getDeclaredField("m")
      field.setAccessible(true)
      val map = field.get(env).asInstanceOf[java.util.Map[String, String]]
      map.put(name, value)
    }

    clearSystemProperty(ENV_CLUSTER_MEMBER_NAME)
    setVariable(ENV_CLUSTER_MEMBER_NAME, "")
  }
}