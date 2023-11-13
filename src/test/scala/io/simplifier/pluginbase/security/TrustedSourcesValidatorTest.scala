package io.simplifier.pluginbase.security

import akka.http.scaladsl.model.StatusCodes.{Forbidden, InternalServerError}
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.server.Directives.{complete, failWith, pass}
import akka.http.scaladsl.server.{RequestContext, StandardRoute}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.simplifier.pluginapi.rest.PluginHeaders._
import io.simplifier.pluginbase.SimplifierPlugin.AppServerInformation
import io.simplifier.pluginbase.definitions.InternalDataHandling.{BlackAndWhiteList, IncomingRequests, Security}
import io.simplifier.pluginbase.interfaces.PluginBaseHttpService
import io.simplifier.pluginbase.util.api.ApiExceptionHandler.handleApiFailures
import io.simplifier.pluginbase.{PluginDescription, PluginSettings}
import org.mockito.MockitoSugar
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.Logger

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

class TrustedSourcesValidatorTest extends AnyWordSpec with Matchers with ScalatestRouteTest {


  "The original request source verifier function" must {
    "responds correctly with default values" when {
      "null values has been provided" in new VerificationFixture {
        noException should be thrownBy validatorWrongConfig1.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig1.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig1.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig1.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig1.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig1.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig1.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig2.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig2.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig2.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig2.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig2.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig2.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig2.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig3.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig3.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig3.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig3.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig3.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig3.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig3.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig4.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig4.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig4.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig4.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig4.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig4.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig4.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig5.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig5.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig5.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig5.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig5.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig5.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig5.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig6.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig6.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig6.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig6.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig6.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig6.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig6.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig7.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig7.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig7.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig7.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig7.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig7.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig7.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig8.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig8.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig8.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig8.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig8.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig8.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig8.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig9.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig9.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig9.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig9.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig9.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig9.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig9.verifyOriginalRequestSource(Plugin("", Some(null)))


        noException should be thrownBy validatorWrongConfig10.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig10.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig10.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig10.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig10.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig10.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig10.verifyOriginalRequestSource(Plugin("", Some(null)))


        noException should be thrownBy validatorWrongConfig11.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig11.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig11.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig11.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig11.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig11.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig11.verifyOriginalRequestSource(Plugin("", Some(null)))


        noException should be thrownBy validatorWrongConfig12.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig12.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig12.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig12.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig12.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig12.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig12.verifyOriginalRequestSource(Plugin("", Some(null)))


        noException should be thrownBy validatorWrongConfig13.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig13.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig13.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig13.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig13.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig13.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig13.verifyOriginalRequestSource(Plugin("", Some(null)))


        noException should be thrownBy validatorWrongConfig14.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig14.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig14.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig14.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig14.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig14.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig14.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig15.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig15.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig15.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig15.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig15.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig15.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig15.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig16.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig16.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig16.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig16.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig16.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig16.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig16.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig17.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig17.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig17.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig17.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig17.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig17.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig17.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorWrongConfig18.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorWrongConfig18.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorWrongConfig18.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorWrongConfig18.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorWrongConfig18.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorWrongConfig18.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorWrongConfig18.verifyOriginalRequestSource(Plugin("", Some(null)))

        noException should be thrownBy validatorPassed1.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorPassed1.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorPassed1.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorPassed1.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorPassed1.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorPassed1.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorPassed1.verifyOriginalRequestSource(Plugin("", Some(null)))
      }

      "a wrong AppServer Prefix been provided" in new VerificationFixture {
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(Plugin("", Some(null)))
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(null)
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(AppServer(null))
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(AppServerDirect(null))
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(AppServerDirect(Some(null)))
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(BusinessObject("", null))
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(Plugin("", null))
        noException should be thrownBy validatorPassed0.verifyOriginalRequestSource(Plugin("", Some(null)))
      }
    }


    "pass" when {
      "untrusted sources are allowed" in new VerificationFixture {
        validatorPassed1.verifyOriginalRequestSource(AppServer(None)) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(AppServer(Some(Uri()))) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(AppServer(Some(Uri("")))) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(AppServerDirect(None)) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(AppServerDirect(Some(Uri()))) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(AppServerDirect(Some(Uri("")))) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(BusinessObject("", None)) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(BusinessObject("", Some(Uri()))) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(BusinessObject("", Some(Uri("")))) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(Plugin("", None)) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(Plugin("", Some(Uri()))) mustBe pass
        validatorPassed1.verifyOriginalRequestSource(Plugin("", Some(Uri("")))) mustBe pass

        validatorPassed2.verifyOriginalRequestSource(AppServer(None)) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(AppServer(Some(Uri()))) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(AppServer(Some(Uri("")))) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(AppServerDirect(None)) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(AppServerDirect(Some(Uri()))) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(AppServerDirect(Some(Uri("")))) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(BusinessObject("", None)) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(BusinessObject("", Some(Uri()))) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(BusinessObject("", Some(Uri("")))) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(Plugin("", None)) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(Plugin("", Some(Uri()))) mustBe pass
        validatorPassed2.verifyOriginalRequestSource(Plugin("", Some(Uri("")))) mustBe pass

        validatorPassed3.verifyOriginalRequestSource(AppServer(None)) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(AppServer(Some(Uri()))) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(AppServer(Some(Uri("")))) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(AppServerDirect(None)) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(AppServerDirect(Some(Uri()))) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(AppServerDirect(Some(Uri("")))) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(BusinessObject("", None)) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(BusinessObject("", Some(Uri()))) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(BusinessObject("", Some(Uri("")))) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(Plugin("", None)) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(Plugin("", Some(Uri()))) mustBe pass
        validatorPassed3.verifyOriginalRequestSource(Plugin("", Some(Uri("")))) mustBe pass

        validatorPassed4.verifyOriginalRequestSource(AppServer(None)) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(AppServer(Some(Uri()))) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(AppServer(Some(Uri("")))) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(AppServerDirect(None)) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(AppServerDirect(Some(Uri()))) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(AppServerDirect(Some(Uri("")))) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(BusinessObject("", None)) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(BusinessObject("", Some(Uri()))) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(BusinessObject("", Some(Uri("")))) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(Plugin("", None)) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(Plugin("", Some(Uri()))) mustBe pass
        validatorPassed4.verifyOriginalRequestSource(Plugin("", Some(Uri("")))) mustBe pass
      }

      "untrusted sources are disallowed, but the Uri is in a whitelist" in new VerificationFixture {
        validatorWhitelist.verifyOriginalRequestSource(AppServer(whiteListedUri)) mustBe pass
        validatorWhitelist.verifyOriginalRequestSource(AppServerDirect(whiteListedUri)) mustBe pass
        validatorWhitelist.verifyOriginalRequestSource(BusinessObject("", whiteListedUri)) mustBe pass
        validatorWhitelist.verifyOriginalRequestSource(Plugin("", whiteListedUri)) mustBe pass
      }

      "untrusted sources are disallowed, but the Uri is not in a blacklist" in new VerificationFixture {
        validatorBlacklist.verifyOriginalRequestSource(AppServer(notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(AppServerDirect(notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(BusinessObject("", notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(Plugin("", notBlackListedUri)) mustBe pass
      }

      "untrusted sources are disallowed, but the Uri is not in a blacklist and a blacklist has been provided" in new VerificationFixture {
        validatorBlacklist.verifyOriginalRequestSource(AppServer(notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(AppServerDirect(notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(BusinessObject("", notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(Plugin("", notBlackListedUri)) mustBe pass
      }


      "untrusted sources are disallowed, but the Uri is whitelisted and not in a blacklist and both lists have been provided" in new VerificationFixture {
        validatorBothList.verifyOriginalRequestSource(AppServer(notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(AppServerDirect(notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(BusinessObject("", notBlackListedUri)) mustBe pass
        validatorBlacklist.verifyOriginalRequestSource(Plugin("", notBlackListedUri)) mustBe pass
      }

      "untrusted sources are disallowed, and the Uri ist the AppServer Uri itself and a blacklist has been provided" in new VerificationFixture {
        validatorBlacklist2.verifyOriginalRequestSource(AppServer(Some(appServerPrefix))) mustBe pass
        validatorBlacklist2.verifyOriginalRequestSource(AppServerDirect(Some(appServerPrefix))) mustBe pass
        validatorBlacklist2.verifyOriginalRequestSource(BusinessObject("", Some(appServerPrefix))) mustBe pass
        validatorBlacklist2.verifyOriginalRequestSource(Plugin("", Some(appServerPrefix))) mustBe pass

        validatorBlacklist3.verifyOriginalRequestSource(AppServer(Some(appServerPrefix))) mustBe pass
        validatorBlacklist3.verifyOriginalRequestSource(AppServerDirect(Some(appServerPrefix))) mustBe pass
        validatorBlacklist3.verifyOriginalRequestSource(BusinessObject("", Some(appServerPrefix))) mustBe pass
        validatorBlacklist3.verifyOriginalRequestSource(Plugin("", Some(appServerPrefix))) mustBe pass
      }

      "untrusted sources are disallowed, and the Uri ist the AppServer Uri itself and a whitelist has been provided" in new VerificationFixture {
        validatorWhitelist2.verifyOriginalRequestSource(AppServer(Some(appServerPrefix))) mustBe pass
        validatorWhitelist2.verifyOriginalRequestSource(AppServerDirect(Some(appServerPrefix))) mustBe pass
        validatorWhitelist2.verifyOriginalRequestSource(BusinessObject("", Some(appServerPrefix))) mustBe pass
        validatorWhitelist2.verifyOriginalRequestSource(Plugin("", Some(appServerPrefix))) mustBe pass
      }


      "untrusted sources are disallowed, and the Uri ist the AppServer Uri itself and both lists have been provided" in new VerificationFixture {
        validatorBothList2.verifyOriginalRequestSource(AppServer(Some(appServerPrefix))) mustBe pass
        validatorBothList2.verifyOriginalRequestSource(AppServerDirect(Some(appServerPrefix))) mustBe pass
        validatorBothList2.verifyOriginalRequestSource(BusinessObject("", Some(appServerPrefix))) mustBe pass
        validatorBothList2.verifyOriginalRequestSource(Plugin("", Some(appServerPrefix))) mustBe pass

        validatorBothList4.verifyOriginalRequestSource(AppServer(Some(appServerPrefix))) mustBe pass
        validatorBothList4.verifyOriginalRequestSource(AppServerDirect(Some(appServerPrefix))) mustBe pass
        validatorBothList4.verifyOriginalRequestSource(BusinessObject("", Some(appServerPrefix))) mustBe pass
        validatorBothList4.verifyOriginalRequestSource(Plugin("", Some(appServerPrefix))) mustBe pass

        validatorBothList6.verifyOriginalRequestSource(AppServer(Some(appServerPrefix))) mustBe pass
        validatorBothList6.verifyOriginalRequestSource(AppServerDirect(Some(appServerPrefix))) mustBe pass
        validatorBothList6.verifyOriginalRequestSource(BusinessObject("", Some(appServerPrefix))) mustBe pass
        validatorBothList6.verifyOriginalRequestSource(Plugin("", Some(appServerPrefix))) mustBe pass
      }


    }

    "return a forbidden status code" when {
      "untrusted sources are disallowed and the Uri is in a blacklist" in new VerificationFixture {
        val settings: PluginSettings = pluginSettings.copy(security = securitySettingsBlacklist)
        Get("/") ~>
        addHeader(`Plugin-Request-Source`(RequestSourceAppServer)) ~>
        addHeader(`Plugin-Secret`(pluginSettings.pluginSecret)) ~>
        addHeader(`Simplifier-Call-Uri`(blackListedUri.map(_.toString).get)) ~>
        handleApiFailures {
          new PluginBaseHttpService(pluginDescription, settings, appServerInformationWithoutPrefix, None, None, None).route
        } ~> check {
          response.status mustBe Forbidden
          entityAs[String] mustBe failureTextBlackList(blackListedUri.get)
        }
      }

      "untrusted sources are disallowed and the Uri is not in a whitelist" in new VerificationFixture {
        val settings: PluginSettings = pluginSettings.copy(security = securitySettingsWhitelist2)
        Get("/") ~>
        addHeader(`Plugin-Request-Source`(RequestSourceAppServer)) ~>
        addHeader(`Plugin-Secret`(pluginSettings.pluginSecret)) ~>
        addHeader(`Simplifier-Call-Uri`(blackListedUri.map(_.toString).get)) ~>
        handleApiFailures {
          new PluginBaseHttpService(pluginDescription, settings, appServerInformationWithoutPrefix, None, None, None).route
        } ~> check {
          response.status mustBe Forbidden
          entityAs[String] mustBe failureTextWhitelist(blackListedUri.get)
        }
      }

      "untrusted sources are disallowed and the Uri is empty and a whitelist has been provided" in new VerificationFixture {
        val settings: PluginSettings = pluginSettings.copy(security = securitySettingsWhitelist2)
        Get("/") ~>
        addHeader(`Plugin-Request-Source`(RequestSourceAppServer)) ~>
        addHeader(`Plugin-Secret`(pluginSettings.pluginSecret)) ~>
        addHeader(`Simplifier-Call-Uri`(Some(Uri("")).map(_.toString).get)) ~>
        handleApiFailures {
          new PluginBaseHttpService(pluginDescription, settings, appServerInformationWithoutPrefix, None, None, None).route
        } ~> check {
          response.status mustBe Forbidden
          entityAs[String] mustBe failureTextWhitelist(Uri(""))
        }
      }


      "untrusted sources are disallowed and the Uri is not whitelisted blacklisted and a both lists have been provided" in new VerificationFixture {
        val settings: PluginSettings = pluginSettings.copy(security = securitySettingsBothList3)
        Get("/") ~>
        addHeader(`Plugin-Request-Source`(RequestSourceAppServer)) ~>
        addHeader(`Plugin-Secret`(pluginSettings.pluginSecret)) ~>
        addHeader(`Simplifier-Call-Uri`(notBlackListedUriString)) ~>
        handleApiFailures {
          new PluginBaseHttpService(pluginDescription, settings, appServerInformationWithoutPrefix, None, None, None).route
        } ~> check {
          response.status mustBe Forbidden
          entityAs[String] mustBe failureTextWhitelist(notBlackListedUriString)
        }
      }

      "untrusted sources are disallowed and the Uri is blacklisted and a both lists have been provided" in new VerificationFixture {
        val settings: PluginSettings = pluginSettings.copy(security = securitySettingsBothList2)
        Get("/") ~>
        addHeader(`Plugin-Request-Source`(RequestSourceAppServer)) ~>
        addHeader(`Plugin-Secret`(pluginSettings.pluginSecret)) ~>
        addHeader(`Simplifier-Call-Uri`(notBlackListedUriString)) ~>
        handleApiFailures {
          new PluginBaseHttpService(pluginDescription, settings, appServerInformationWithoutPrefix, None, None, None).route
        } ~> check {
          response.status mustBe Forbidden
          entityAs[String] mustBe failureTextBlacklistedAlbeitWhitelist(notBlackListedUriString)
        }
      }
    }


    //TODO Check Exception How?
    "return an exception" when {
      "a wrong configuration has been provided" in new VerificationFixture {
        val settings: PluginSettings = pluginSettings.copy(security = securitySettings5)
        Get("/") ~>
        addHeader(`Plugin-Request-Source`(RequestSourceAppServer)) ~>
        addHeader(`Plugin-Secret`(pluginSettings.pluginSecret)) ~>
        addHeader(`Simplifier-Call-Uri`(blackListedUri.map(_.toString).get)) ~>
        new PluginBaseHttpService(pluginDescription, settings, appServerInformationWithoutPrefix, None, None, None).route ~>
        check {
          response.status mustBe InternalServerError
        }
      }
    }
  }

  "The isListed function" must {
    "return false" when {
      "a lookup of a domain source mismatches due to errors" in new ExceptionFixture {
        validator.isListed(uriWithPort, Seq(crippleMasterUri)) mustBe false
      }

      "a lookup of an IPv4 source mismatches due to errors" in new ExceptionFixtureIPv4 {
        validator.isListed(uriWithPortIPv4, Seq("127.0.0.1:88888")) mustBe false
        validator.isListed(uriWithPortIPv4, Seq(crippleMasterUriIPv4)) mustBe false
      }

      "a lookup in a pure trusted source sequence happen and the URI is not listed" in new BaseFixture {
        validator.isListed(Uri(""), Seq("127.0.0.1", "127.0.0.2")) mustBe false
        validator.isListed(Uri(), Seq("127.0.0.1", "127.0.0.2")) mustBe false
        validator.isListed(Uri("127.0.0.3"), Seq("127.0.0.1", "127.0.0.2")) mustBe false
        validator.isListed(Uri(""), Seq("test", "test.domain")) mustBe false
        validator.isListed(Uri(), Seq("test", "test.domain")) mustBe false
        validator.isListed(Uri("test2"), Seq("test", "test.domain")) mustBe false
        validator.isListed(Uri("test.domain2"), Seq("test", "test.domain")) mustBe false
      }

      "a lookup in a mixed trusted source sequence happen and the URI is not listed" in new BaseFixture {
        validator.isListed(Uri(""), Seq("127.0.0.1", "127.0.0.2", "test", "test.domain")) mustBe false
        validator.isListed(Uri(), Seq("127.0.0.1", "127.0.0.2", "test", "test.domain")) mustBe false
        validator.isListed(Uri("127.0.0.3"), Seq("127.0.0.1", "127.0.0.2", "test", "test.domain")) mustBe false
        validator.isListed(Uri(""), Seq("test", "test.domain", "test", "test.domain")) mustBe false
        validator.isListed(Uri(), Seq("test", "test.domain", "test", "test.domain")) mustBe false
        validator.isListed(Uri("test2"), Seq("test", "test.domain", "test", "test.domain")) mustBe false
        validator.isListed(Uri("test.domain2"), Seq("test", "test.domain", "test", "test.domain")) mustBe false
      }
    }

    "return true" when {
      "a lookup in a pure trusted source sequence happen and the URI is listed" in new BaseFixture {
        validator.isListed(Uri("127.0.0.1"), Seq("127.0.0.1", "127.0.0.2")) mustBe true
        validator.isListed(Uri("127.0.0.2"), Seq("127.0.0.1", "127.0.0.2")) mustBe true
        validator.isListed(Uri("127.0.0.1"), Seq("127.0.0.2", "127.0.0.1")) mustBe true
        validator.isListed(Uri("127.0.0.2"), Seq("127.0.0.1", "127.0.0.2")) mustBe true
        validator.isListed(Uri("test"), Seq("test", "test.domain")) mustBe true
        validator.isListed(Uri("test.domain"), Seq("test", "test.domain")) mustBe true
        validator.isListed(Uri("test"), Seq("test.domain", "test")) mustBe true
        validator.isListed(Uri("test.domain"), Seq("test", "test.domain")) mustBe true
      }

      "a lookup in a mixed trusted source sequence happen and the URI is listed" in new BaseFixture {
        validator.isListed(Uri("127.0.0.1"), Seq("127.0.0.1", "127.0.0.2", "test", "test.domain")) mustBe true
        validator.isListed(Uri("127.0.0.2"), Seq("127.0.0.1", "127.0.0.2", "test", "test.domain")) mustBe true
        validator.isListed(Uri("test"), Seq("test", "test.domain", "test", "test.domain")) mustBe true
        validator.isListed(Uri("test.domain"), Seq("test", "test.domain", "test", "test.domain")) mustBe true
      }
    }
  }


  "The trusted IPv4 validator" must {
    "return false" when {
      "provided values which lead to an exception" in new ExceptionFixtureIPv4 {
        validator.matchesTrust("127.0.0.1:989\"9\"8", uriWithPortIPv4, IPv4Warning) mustBe false
        // verify(loggerMockIPv4, times(1)).warn(expectedLogIPv4)
      }

      "provided null values" in new BaseFixture {
        validator.matchesTrust(null, Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("", null, IPv4Warning) mustBe false
        validator.matchesTrust(null, null, IPv4Warning) mustBe false
      }

      "provided an empty Uri" in new BaseFixture {
        validator.matchesTrust("*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.0.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.*.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.0.*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.0.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.1", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.*.*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.0.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.*.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.0.*:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.0.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.*:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.*:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.*.*:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.*:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.*:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.0.1:*", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.0.1:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.*.1:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.0.*:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.0.1:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.1:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.1:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.1:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.*:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.1:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.*.*:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.*.*:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.*:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("*.0.0.*:60", Uri(), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.0.1:60", Uri(), IPv4Warning) mustBe false
      }

      "provided a Uri with an untrusted scheme" in new BaseFixture {
        validator.matchesTrust("https://127.0.0.1", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("https://127.0.0.1:60", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe false
        validator.matchesTrust("https://*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("https://*:60", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe false
        validator.matchesTrust("https://*:*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
      }

      "provided a Uri with an untrusted port" in new BaseFixture {
        validator.matchesTrust("http://127.0.0.1:60", Uri("").withHost("127.0.0.1").withScheme("http").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("*://127.0.0.1:60", Uri("").withHost("127.0.0.1").withScheme("http").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("http://*:60", Uri("").withHost("127.0.0.1").withScheme("http").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("*://*:60", Uri("").withHost("127.0.0.1").withScheme("http").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("http://127.0.0.1:60", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("*://127.0.0.1:60", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("http://*:60", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("*://*:60", Uri("").withHost("127.0.0.1").withPort(6), IPv4Warning) mustBe false
      }

      "provided a Uri with one or more untrusted parts" in new BaseFixture {
        validator.matchesTrust("127.0.0.2", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.*.2", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.2", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe false
        validator.matchesTrust("http://127.0.0.2", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("http://127.*.*.2", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("http://*.*.*.2", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("*://127.0.0.2", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("*://127.*.*.2", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("*://*.*.*.2", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.0.2:60", Uri("").withHost("127.0.0.1").withPort(60), IPv4Warning) mustBe false
        validator.matchesTrust("127.*.*.2:60", Uri("").withHost("127.0.0.1").withPort(60), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.*.2:60", Uri("").withHost("127.0.0.1").withPort(60), IPv4Warning) mustBe false
        validator.matchesTrust("127.0.0.2:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("128.*.*.*:*", Uri("").withHost("127.0.0.2").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("*.*.1.*.*:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("128.*.*.*:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("*.1.*.*:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe false
        validator.matchesTrust("*://*.*.*.*:60", Uri("").withHost("127.0.0.1").withPort(61).withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("http://127.0.0.1:60", Uri("").withHost("127.0.0.1").withPort(61).withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("*://*.*.0.*:60", Uri("").withHost("127.0.0.1").withPort(61).withScheme("http"), IPv4Warning) mustBe false
        validator.matchesTrust("*://*.*.*.1:60", Uri("").withHost("127.0.0.1").withPort(61).withScheme("http"), IPv4Warning) mustBe false
      }
    }

    "return true" when {
      "provided a Uri and wildcard trusted sources" in new BaseFixture {
        validator.matchesTrust("*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*.*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*:*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*:*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*:*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*:*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*:*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*:*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*:*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*.*:*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*:*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*:*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*:*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*:*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*:*", Uri("").withHost("127.0.0.3"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*:*", Uri("").withHost("127.0.0.2").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*:*", Uri("").withHost("127.0.0.4").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*.*.*.*:*", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("*://*:*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*://*:*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*:*", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*:*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*:*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*:*", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*:*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*:*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*:*", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*:*", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*:*", Uri("").withHost("127.0.0.1").withPort(61), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*:*", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("*://*.*.*.*:*", Uri("").withHost("127.0.0.1").withScheme("http").withPort(60), IPv4Warning) mustBe true
      }

      "provided a Uri with all trusted parts" in new BaseFixture {
        validator.matchesTrust("127.0.0.1", Uri("").withHost("127.0.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("127.11.0.1", Uri("").withHost("127.11.0.1"), IPv4Warning) mustBe true
        validator.matchesTrust("http://127.0.0.1", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("http://127.11.0.1", Uri("").withHost("127.11.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("127.0.0.1:60", Uri("").withHost("127.0.0.1").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("127.11.0.1:60", Uri("").withHost("127.11.0.1").withPort(60), IPv4Warning) mustBe true
        validator.matchesTrust("http://127.0.0.1", Uri("").withHost("127.0.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("http://127.11.0.1", Uri("").withHost("127.11.0.1").withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("http://127.0.0.1:60", Uri("").withHost("127.0.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
        validator.matchesTrust("http://127.11.0.1:60", Uri("").withHost("127.11.0.1").withPort(60).withScheme("http"), IPv4Warning) mustBe true
      }
    }
  }


  "The trusted domain validator" must {
    "return false" when {
      "provided values which lead to an exception" in new ExceptionFixture {
        validator.matchesTrust(crippleMasterUri, uriWithPort, DomainWarning) mustBe false
        //  verify(loggerMock, times(1)).warn(expectedLog)
      }

      "provided null values" in new BaseFixture {
        validator.matchesTrust(null, Uri(), DomainWarning) mustBe false
        validator.matchesTrust("", null, DomainWarning) mustBe false
        validator.matchesTrust(null, null, DomainWarning) mustBe false
      }

      "provided an empty Uri" in new BaseFixture {
        validator.matchesTrust("*", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("test", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("test:60", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("http://test", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("http://test:60", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("http://*", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("http://*:60", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("*://*", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("*://test", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("*://test:60", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("*://test:*", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("*://*:60", Uri(), DomainWarning) mustBe false
        validator.matchesTrust("*://*:*", Uri(), DomainWarning) mustBe false
      }

      "provided a Uri with an untrusted scheme" in new BaseFixture {
        validator.matchesTrust("https://test", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("https://test:60", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("https://*", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("https://*:60", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("https://*:*", Uri("http://test"), DomainWarning) mustBe false
      }

      "provided a Uri with an untrusted port" in new BaseFixture {
        validator.matchesTrust("http://test:60", Uri("http://test:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://test:60", Uri("http://test:61"), DomainWarning) mustBe false
        validator.matchesTrust("http://*:60", Uri("http://test:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://*:60", Uri("http://test:61"), DomainWarning) mustBe false
        validator.matchesTrust("http://test:60", Uri("test:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://test:60", Uri("test:61"), DomainWarning) mustBe false
        validator.matchesTrust("http://*:60", Uri("test:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://*:60", Uri("test:61"), DomainWarning) mustBe false
      }

      "provided a Uri with one or more untrusted parts" in new BaseFixture {
        validator.matchesTrust("test", Uri("test.domain"), DomainWarning) mustBe false
        validator.matchesTrust("test.domain", Uri("test"), DomainWarning) mustBe false
        validator.matchesTrust("test.*", Uri("test"), DomainWarning) mustBe false
        validator.matchesTrust("test.domain", Uri("test"), DomainWarning) mustBe false
        validator.matchesTrust("*.domain", Uri("domain.test"), DomainWarning) mustBe false
        validator.matchesTrust("http://test", Uri("http://test.domain"), DomainWarning) mustBe false
        validator.matchesTrust("http://test.domain", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("http://test.*", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("http://*.domain", Uri("http://domain.test"), DomainWarning) mustBe false
        validator.matchesTrust("*://test", Uri("http://test.domain"), DomainWarning) mustBe false
        validator.matchesTrust("*://test.domain", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("*://test.*", Uri("http://test"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.domain", Uri("http://domain.test"), DomainWarning) mustBe false
        validator.matchesTrust("test:60", Uri("test.domain:60"), DomainWarning) mustBe false
        validator.matchesTrust("test.domain:60", Uri("test:60"), DomainWarning) mustBe false
        validator.matchesTrust("test.*:60", Uri("test:60"), DomainWarning) mustBe false
        validator.matchesTrust("*.domain:60", Uri("domain.test:60"), DomainWarning) mustBe false
        validator.matchesTrust("test:*", Uri("test.domain:61"), DomainWarning) mustBe false
        validator.matchesTrust("test.domain:*", Uri("test:61"), DomainWarning) mustBe false
        validator.matchesTrust("test.*:*", Uri("test:61"), DomainWarning) mustBe false
        validator.matchesTrust("test.domain:*", Uri("test:61"), DomainWarning) mustBe false
        validator.matchesTrust("*.domain:*", Uri("domain.test:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*:*", Uri("http://test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*:*", Uri("test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*:*", Uri("http://test.tetz:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*:*", Uri("test.tetz:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*", Uri("http://test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*", Uri("test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*", Uri("http://test.tetz:61"), DomainWarning) mustBe false
        validator.matchesTrust("*://*.*.*", Uri("test.tetz:61"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*:*", Uri("http://test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*:*", Uri("test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*:*", Uri("http://test.tetz:61"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*:*", Uri("test.tetz:61"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*", Uri("http://test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*", Uri("test.tetz"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*", Uri("http://test.tetz:61"), DomainWarning) mustBe false
        validator.matchesTrust("*.*.*", Uri("test.tetz:61"), DomainWarning) mustBe false
      }
    }

    "return true" when {
      "provided a Uri and wildcard trusted sources" in new BaseFixture {
        validator.matchesTrust("*", Uri("http://test:61"), DomainWarning) mustBe true
        validator.matchesTrust("*", Uri("http://test"), DomainWarning) mustBe true
        validator.matchesTrust("*", Uri("test"), DomainWarning) mustBe true
        validator.matchesTrust("*", Uri("test:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://test", Uri("http://test"), DomainWarning) mustBe true
        validator.matchesTrust("*://test", Uri("test"), DomainWarning) mustBe true
        validator.matchesTrust("*://test:*", Uri("http://test"), DomainWarning) mustBe true
        validator.matchesTrust("*://test:*", Uri("test"), DomainWarning) mustBe true
        validator.matchesTrust("*://test:*", Uri("http://test:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://test:*", Uri("test:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("http://test.tez"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("test.tez"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("http://test.tez:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("test.tez:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("http://test.tez.t1000"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("test.tez.t1000"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("http://test.tez.t1000:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*", Uri("test.tez.t1000:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("http://test.tetz.tez"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("test.tetz.tez"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("http://test.tetz.tez:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*.*.*", Uri("test.tetz.tez:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*", Uri("http://test"), DomainWarning) mustBe true
        validator.matchesTrust("*://*", Uri("test"), DomainWarning) mustBe true
        validator.matchesTrust("*://*:*", Uri("http://test"), DomainWarning) mustBe true
        validator.matchesTrust("*://*:*", Uri("test"), DomainWarning) mustBe true
        validator.matchesTrust("*://*:*", Uri("http://test:61"), DomainWarning) mustBe true
        validator.matchesTrust("*://*:*", Uri("test:61"), DomainWarning) mustBe true
      }

      "provided a Uri with all trusted parts" in new BaseFixture {
        validator.matchesTrust("test", Uri("test"), DomainWarning) mustBe true
        validator.matchesTrust("test.domain", Uri("test.domain"), DomainWarning) mustBe true
        validator.matchesTrust("http://test", Uri("http://test"), DomainWarning) mustBe true
        validator.matchesTrust("http://test.domain", Uri("http://test.domain"), DomainWarning) mustBe true
        validator.matchesTrust("test:60", Uri("test:60"), DomainWarning) mustBe true
        validator.matchesTrust("test.domain:60", Uri("test.domain:60"), DomainWarning) mustBe true
        validator.matchesTrust("http://test", Uri("http://test"), DomainWarning) mustBe true
        validator.matchesTrust("http://test.domain", Uri("http://test.domain"), DomainWarning) mustBe true
        validator.matchesTrust("http://test:60", Uri("http://test:60"), DomainWarning) mustBe true
        validator.matchesTrust("http://test.domain:60", Uri("http://test.domain:60"), DomainWarning) mustBe true
      }
    }
  }


  "The validate parts method" must {
    "return false" when {
      "provided null values" in new BaseFixture {
        validator.validateParts(null, null) mustBe false
        validator.validateParts(null, Seq()) mustBe false
        validator.validateParts(Seq(), null) mustBe false
        validator.validateParts(Seq(null), Seq()) mustBe false
        validator.validateParts(Seq(), Seq(null)) mustBe false
        validator.validateParts(Seq(null), Seq(null)) mustBe false
        validator.validateParts(Seq(null), Seq("*")) mustBe false
        validator.validateParts(Seq("test"), Seq(null)) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq(null)) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq(null, "*")) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq("*", null)) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq(null, null)) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq(null, null, null)) mustBe false
      }

      "provided an empty value" in new BaseFixture {
        validator.validateParts(Seq(), Seq()) mustBe false
        validator.validateParts(Seq(), Seq("test")) mustBe false
        validator.validateParts(Seq(), Seq("*")) mustBe false
        validator.validateParts(Seq("test"), Seq()) mustBe false
      }

      "provided a value, which is not trusted" in new BaseFixture {
        validator.validateParts(Seq("test", "domain"), Seq("test", "domain2")) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq("test2", "domain")) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq("test2", "*")) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq("*", "domain2")) mustBe false
      }

      "provided a value, which is trusted, but the trusted sequence requires more values" in new BaseFixture {
        validator.validateParts(Seq("test", "domain"), Seq("*", "*", "*")) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq("test", "*", "*")) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq("*", "domain", "*")) mustBe false
        validator.validateParts(Seq("test", "domain"), Seq("test", "domain", "*")) mustBe false
      }
    }

    "return true" when {
      "provided a value, which is trusted" in new BaseFixture {
        validator.validateParts(Seq("test"), Seq("*")) mustBe true
        validator.validateParts(Seq("test", "domain"), Seq("*")) mustBe true
        validator.validateParts(Seq("test", "domain"), Seq("*", "*")) mustBe true
        validator.validateParts(Seq("test", "domain"), Seq("test", "*")) mustBe true
        validator.validateParts(Seq("test", "domain"), Seq("*", "domain")) mustBe true
        validator.validateParts(Seq("test", "domain"), Seq("test", "domain")) mustBe true
      }
    }
  }


  "The wildcard checker" must {
    "return the correct value" when {
      "provided a value, which is a wildcard" in new BaseFixture {
        validator.isOnlyWildcard(null) mustBe false
        validator.isOnlyWildcard(Seq(null)) mustBe false
        validator.isOnlyWildcard(Seq(null, "*")) mustBe false
        validator.isOnlyWildcard(isOnlyWildcard1) mustBe true
        validator.isOnlyWildcard(isOnlyWildcard2) mustBe true
        validator.isOnlyWildcard(isOnlyWildcard3) mustBe true
        validator.isOnlyWildcard(isNotOnlyWildcard1) mustBe false
        validator.isOnlyWildcard(isNotOnlyWildcard2) mustBe false
        validator.isOnlyWildcard(isNotOnlyWildcard3) mustBe false
        validator.isOnlyWildcard(isNotOnlyWildcard4) mustBe false
      }
    }
  }


  "The Uri sanitizer" must {
    "return the correct value" when {
      "provided a value, which is either null or empty" in new BaseFixture {
        validator.sanitizeUri(null) mustBe emptyString
        validator.sanitizeUri(Left(null)) mustBe emptyString
        validator.sanitizeUri(Right(null)) mustBe emptyString
        validator.sanitizeUri(Left("")) mustBe emptyString
        validator.sanitizeUri(Right(Uri())) mustBe emptyString
      }

      "provided a correct value" in new BaseFixture {
        validator.sanitizeUri(Left("  ")) mustBe ""
        validator.sanitizeUri(Left("  http://127 ")) mustBe "http://127"
        validator.sanitizeUri(Left(" 127.0.0.1 ")) mustBe "127.0.0.1"
        validator.sanitizeUri(Left(" http://127.0.0.1 ")) mustBe "http://127.0.0.1"
        validator.sanitizeUri(Left(" http://127.0.0.1:50 ")) mustBe "http://127.0.0.1:50"
        validator.sanitizeUri(Left(" hTTp://test ")) mustBe "hTTp://test"
        validator.sanitizeUri(Left(" hTTp://test:50 ")) mustBe "hTTp://test:50"
        validator.sanitizeUri(Left(" https://127.0.0.1 ")) mustBe "https://127.0.0.1"
        validator.sanitizeUri(Left(" httpS://127.0.0.1:50 ")) mustBe "httpS://127.0.0.1:50"
        validator.sanitizeUri(Left(" hTTps://test ")) mustBe "hTTps://test"
        validator.sanitizeUri(Left(" hTTpS://test:50 ")) mustBe "hTTpS://test:50"
        validator.sanitizeUri(Left(" ws://127.0.0.1 ")) mustBe "ws://127.0.0.1"
        validator.sanitizeUri(Left(" ws://127.0.0.1:50 ")) mustBe "ws://127.0.0.1:50"
        validator.sanitizeUri(Left(" wS://test ")) mustBe "wS://test"
        validator.sanitizeUri(Left(" Ws://test:50 ")) mustBe "Ws://test:50"
        validator.sanitizeUri(Left(" wss://127.0.0.1 ")) mustBe "wss://127.0.0.1"
        validator.sanitizeUri(Left(" WsS://127.0.0.1:50 ")) mustBe "WsS://127.0.0.1:50"
        validator.sanitizeUri(Left(" wSs://test ")) mustBe "wSs://test"
        validator.sanitizeUri(Left(" WSS://test:50 ")) mustBe "WSS://test:50"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withScheme("http"))) mustBe "http://127.0.0.1"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("Http"))) mustBe "http://127.0.0.1:60"
        validator.sanitizeUri(Right(Uri().withHost("test").withScheme("http"))) mustBe "http://test"
        validator.sanitizeUri(Right(Uri().withHost("test").withPort(60).withScheme("HTTP"))) mustBe "http://test:60"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withScheme("httpS"))) mustBe "https://127.0.0.1"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("hTtpS"))) mustBe "https://127.0.0.1:60"
        validator.sanitizeUri(Right(Uri().withHost("test").withScheme("HTTPs"))) mustBe "https://test"
        validator.sanitizeUri(Right(Uri().withHost("test").withPort(60).withScheme("HtTpS"))) mustBe "https://test:60"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withScheme("WS"))) mustBe "ws://127.0.0.1"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("Ws"))) mustBe "ws://127.0.0.1:60"
        validator.sanitizeUri(Right(Uri().withHost("test").withScheme("ws"))) mustBe "ws://test"
        validator.sanitizeUri(Right(Uri().withHost("test").withPort(60).withScheme("wS"))) mustBe "ws://test:60"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withScheme("WsS"))) mustBe "wss://127.0.0.1"
        validator.sanitizeUri(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("wss"))) mustBe "wss://127.0.0.1:60"
        validator.sanitizeUri(Right(Uri().withHost("test").withScheme("wSS"))) mustBe "wss://test"
        validator.sanitizeUri(Right(Uri().withHost("test").withPort(60).withScheme("WSS"))) mustBe "wss://test:60"
      }
    }
  }


  "The scheme extractor" must {
    "return the correct value" when {
      "provided a value, which is either null or empty" in new BaseFixture {
        validator.extractScheme(null) mustBe None
        validator.extractScheme(Left(null)) mustBe None
        validator.extractScheme(Right(null)) mustBe None
        validator.extractScheme(Left("")) mustBe None
        validator.extractScheme(Right(Uri())) mustBe None
      }

      "provided a value, which does not contain a scheme" in new BaseFixture {
        validator.extractScheme(Left("127.0.0.1")) mustBe None
        validator.extractScheme(Left("127.0.0.1:50")) mustBe None
        validator.extractScheme(Left("test")) mustBe None
        validator.extractScheme(Left("test:50")) mustBe None
        validator.extractScheme(Right(Uri().withHost("127.0.0.1"))) mustBe None
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withPort(60))) mustBe None
        validator.extractScheme(Right(Uri().withHost("test"))) mustBe None
        validator.extractScheme(Right(Uri().withHost("test").withPort(60))) mustBe None
      }

      "provided a value, which contains a scheme" in new BaseFixture {
        validator.extractScheme(Left(" http://127.0.0.1")) mustBe Some("http")
        validator.extractScheme(Left(" http://127.0.0.1:50")) mustBe Some("http")
        validator.extractScheme(Left(" hTTp://test")) mustBe Some("http")
        validator.extractScheme(Left(" hTTp://test:50")) mustBe Some("http")
        validator.extractScheme(Left(" https://127.0.0.1")) mustBe Some("https")
        validator.extractScheme(Left(" httpS://127.0.0.1:50")) mustBe Some("https")
        validator.extractScheme(Left(" hTTps://test")) mustBe Some("https")
        validator.extractScheme(Left(" hTTpS://test:50")) mustBe Some("https")
        validator.extractScheme(Left(" ws://127.0.0.1")) mustBe Some("ws")
        validator.extractScheme(Left(" ws://127.0.0.1:50")) mustBe Some("ws")
        validator.extractScheme(Left(" wS://test")) mustBe Some("ws")
        validator.extractScheme(Left(" Ws://test:50")) mustBe Some("ws")
        validator.extractScheme(Left(" wss://127.0.0.1")) mustBe Some("wss")
        validator.extractScheme(Left(" WsS://127.0.0.1:50")) mustBe Some("wss")
        validator.extractScheme(Left(" wSs://test")) mustBe Some("wss")
        validator.extractScheme(Left(" WSS://test:50")) mustBe Some("wss")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withScheme("http"))) mustBe Some("http")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("Http"))) mustBe Some("http")
        validator.extractScheme(Right(Uri().withHost("test").withScheme("http"))) mustBe Some("http")
        validator.extractScheme(Right(Uri().withHost("test").withPort(60).withScheme("HTTP"))) mustBe Some("http")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withScheme("httpS"))) mustBe Some("https")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("hTtpS"))) mustBe Some("https")
        validator.extractScheme(Right(Uri().withHost("test").withScheme("HTTPs"))) mustBe Some("https")
        validator.extractScheme(Right(Uri().withHost("test").withPort(60).withScheme("HtTpS"))) mustBe Some("https")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withScheme("WS"))) mustBe Some("ws")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("Ws"))) mustBe Some("ws")
        validator.extractScheme(Right(Uri().withHost("test").withScheme("ws"))) mustBe Some("ws")
        validator.extractScheme(Right(Uri().withHost("test").withPort(60).withScheme("wS"))) mustBe Some("ws")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withScheme("WsS"))) mustBe Some("wss")
        validator.extractScheme(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("wss"))) mustBe Some("wss")
        validator.extractScheme(Right(Uri().withHost("test").withScheme("wSS"))) mustBe Some("wss")
        validator.extractScheme(Right(Uri().withHost("test").withPort(60).withScheme("WSS"))) mustBe Some("wss")
      }
    }
  }

  "The port extractor" must {
    "return the correct value" when {
      "provided a value, which is either null or empty" in new BaseFixture {
        validator.extractPort(null) mustBe None
        validator.extractPort(Left(null)) mustBe None
        validator.extractPort(Right(null)) mustBe None
        validator.extractPort(Left("")) mustBe None
        validator.extractPort(Right(Uri())) mustBe None
        validator.extractPort(Left("127.0.0.1")) mustBe None
        validator.extractPort(Left("test")) mustBe None
        validator.extractPort(Right(Uri().withHost("127.0.0.1"))) mustBe None
        validator.extractPort(Right(Uri().withHost("test"))) mustBe None
      }

      "provided a value, which does not contain a port" in new BaseFixture {
        validator.extractPort(Left("127.0.0.1")) mustBe None
        validator.extractPort(Left("test")) mustBe None
        validator.extractPort(Right(Uri().withHost("127.0.0.1"))) mustBe None
        validator.extractPort(Right(Uri().withHost("test"))) mustBe None
      }

      "provided a value, which contains a port" in new BaseFixture {
        validator.extractPort(Left(" 127.0.0.1:50 ")) mustBe Some("50")
        validator.extractPort(Left(" http://127.0.0.1:50")) mustBe Some("50")
        validator.extractPort(Left("  test:50 ")) mustBe Some("50")
        validator.extractPort(Left(" http://test:50 ")) mustBe Some("50")
        validator.extractPort(Left("  127.0.0.1:50")) mustBe Some("50")
        validator.extractPort(Left(" https://127.0.0.1: 50")) mustBe Some("50")
        validator.extractPort(Left("  test: 50")) mustBe Some("50")
        validator.extractPort(Left(" https://test: 50")) mustBe Some("50")
        validator.extractPort(Left("  127.0.0.1: 50")) mustBe Some("50")
        validator.extractPort(Left(" ws://127.0.0.1: 50")) mustBe Some("50")
        validator.extractPort(Left(" ws://test: 50")) mustBe Some("50")
        validator.extractPort(Left(" wss://127.0.0.1: 50")) mustBe Some("50")
        validator.extractPort(Left(" wss://test: 50 ")) mustBe Some("50")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("Http"))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60).withScheme("HTTP"))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("hTtpS"))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60).withScheme("HtTpS"))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("Ws"))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60).withScheme("wS"))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("WsS"))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("127.0.0.1").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60))) mustBe Some("60")
        validator.extractPort(Right(Uri().withHost("test").withPort(60).withScheme("WSS"))) mustBe Some("60")
      }
    }
  }

  "The Uri splitter" must {
    "return the correct value" when {
      "provided a value, which is either null or empty" in new BaseFixture {
        validator.splitUri(null) mustBe Seq()
        validator.splitUri(Left(null)) mustBe Seq()
        validator.splitUri(Right(null)) mustBe Seq()
        validator.splitUri(Left("")) mustBe Seq()
        validator.splitUri(Left("http://")) mustBe Seq()
        validator.splitUri(Left(":60")) mustBe Seq()
        validator.splitUri(Left("http://:60")) mustBe Seq()
        validator.splitUri(Right(Uri())) mustBe Seq()
        validator.splitUri(Right(Uri().withPort(60))) mustBe Seq()
        validator.splitUri(Right(Uri().withScheme("http"))) mustBe Seq()
        validator.splitUri(Right(Uri().withPort(60).withScheme("http"))) mustBe Seq()
      }

      "provided a correct value" in new BaseFixture {
        validator.splitUri(Left("127.0.0.1")) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Left("http://127.0.0.1")) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Left("127.0.0.1:60")) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Left("http://127.0.0.1:60")) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Left("127")) mustBe Seq("127")
        validator.splitUri(Left("http://127")) mustBe Seq("127")
        validator.splitUri(Left("127:60")) mustBe Seq("127")
        validator.splitUri(Left("http://127:60")) mustBe Seq("127")
        validator.splitUri(Right(Uri().withHost("127.0.0.1"))) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Right(Uri().withHost("127.0.0.1").withPort(60))) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Right(Uri().withHost("127.0.0.1").withScheme("http"))) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("http"))) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Right(Uri().withHost("127.0.0.1"))) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Right(Uri().withHost("127.0.0.1").withPort(60))) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Right(Uri().withHost("127.0.0.1").withScheme("http"))) mustBe Seq("127", "0", "0", "1")
        validator.splitUri(Right(Uri().withHost("127.0.0.1").withPort(60).withScheme("http"))) mustBe Seq("127", "0", "0", "1")
      }
    }
  }



  trait VerificationFixture extends BaseFixture {

    def failureTextBlackList(uri: Uri): String = s"The source: {${uri.toString()}} is blacklisted."
    def failureTextWhitelist(uri: Uri): String = s"The source: {${uri.toString()}} is not part of the provided white list."
    def failureTextBlacklistedAlbeitWhitelist(uri: Uri): String = s"The source: {${uri.toString()}} albeit being in the whitelist is blacklisted."


    def failureBothLists: StandardRoute = failWith(new RuntimeException("The configuration is wrong, either a black list or a white list must be provided, but not both."))
    def failureNoLists: StandardRoute = failWith(new RuntimeException("When the source verification is activated, then a blacklist or whitelist must be provided."))

    def failureBlacklisted(uri: Uri): StandardRoute = complete(Forbidden -> failureTextBlackList(uri))
    def failureNotInWhiteList(uri: Uri): StandardRoute = complete(Forbidden -> failureTextWhitelist(uri))

    val blackListedUri: Option[Uri] = Some(Uri("test"))
    val whiteListedUri: Option[Uri] = Some(Uri("test"))
    val notBlackListedUri: Option[Uri] = Some(Uri("test1"))
    val notWhiteListedUri: Option[Uri] = Some(Uri("test1"))


    val notBlackListedUriString: String = "test1"

    val configurationMishapSettings1: Security = null
    val configurationMishapSettings2: Security = Security(null)
    val configurationMishapSettings3: Security = Security(IncomingRequests(null, allowUntrustedSources = true))
    val configurationMishapSettings4: Security = Security(IncomingRequests(BlackAndWhiteList(null, null), allowUntrustedSources = true))
    val configurationMishapSettings5: Security = Security(IncomingRequests(BlackAndWhiteList(null, Seq()), allowUntrustedSources = true))
    val configurationMishapSettings6: Security = Security(IncomingRequests(BlackAndWhiteList(null, Seq(null)), allowUntrustedSources = true))
    val configurationMishapSettings7: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), null), allowUntrustedSources = true))
    val configurationMishapSettings8: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(null), null), allowUntrustedSources = true))


    val configurationMishapSettings9: Security = Security(IncomingRequests(null, allowUntrustedSources = false))
    val configurationMishapSettings10: Security = Security(IncomingRequests(BlackAndWhiteList(null, null), allowUntrustedSources = false))
    val configurationMishapSettings11: Security = Security(IncomingRequests(BlackAndWhiteList(null, Seq()), allowUntrustedSources = false))
    val configurationMishapSettings12: Security = Security(IncomingRequests(BlackAndWhiteList(null, Seq(null)), allowUntrustedSources = false))
    val configurationMishapSettings13: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), null), allowUntrustedSources = false))
    val configurationMishapSettings14: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(null), null), allowUntrustedSources = false))


    val appServerInformationMishap1: AppServerInformation = AppServerInformation(null, None)
    val appServerInformationMishap2: AppServerInformation = AppServerInformation(Some(null), None)
    val appServerInformationMishap3: AppServerInformation = AppServerInformation(Some(""), None)
    val appServerInformationWithoutPrefix: AppServerInformation = AppServerInformation(None, None)
    val appServerInformationWithFaultyPrefix: AppServerInformation = AppServerInformation(Some("httj://10.10.20.10:test"), None)
    val appServerInformationWithPrefix: AppServerInformation = AppServerInformation(Some(appServerPrefix), None)


    val securitySettings1: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), Seq()), allowUntrustedSources = true))
    val securitySettings2: Security = Security(IncomingRequests(BlackAndWhiteList(Seq("test"), Seq()), allowUntrustedSources = true))
    val securitySettings3: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), Seq("test")), allowUntrustedSources = true))
    val securitySettings4: Security = Security(IncomingRequests(BlackAndWhiteList(Seq("test"), Seq("test")), allowUntrustedSources = true))
    val securitySettings5: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), Seq()), allowUntrustedSources = false))

    val securitySettingsWrongConfig1: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), Seq()), allowUntrustedSources = false))
    val securitySettingsWrongConfig2: Security = Security(IncomingRequests(BlackAndWhiteList(Seq("test"), Seq("test")), allowUntrustedSources = false))

    val securitySettingsBlacklist: Security = Security(IncomingRequests(BlackAndWhiteList(Seq("test"), Seq()), allowUntrustedSources = false))
    val securitySettingsBlacklist2: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(appServerPrefix), Seq()), allowUntrustedSources = false))
    val securitySettingsWhitelist: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), Seq("test")), allowUntrustedSources = false))
    val securitySettingsWhitelist2: Security = Security(IncomingRequests(BlackAndWhiteList(Seq(), Seq("test2")), allowUntrustedSources = false))

    val securitySettingsBothList: Security = Security(IncomingRequests(BlackAndWhiteList(Seq("test2"), Seq("test1")), allowUntrustedSources = false))
    val securitySettingsBothList2: Security = Security(IncomingRequests(BlackAndWhiteList(Seq("test1"), Seq("test1")), allowUntrustedSources = false))
    val securitySettingsBothList3: Security = Security(IncomingRequests(BlackAndWhiteList(Seq("test1"), Seq("test2")), allowUntrustedSources = false))


    val validatorWrongConfig1: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings1), appServerInformationWithoutPrefix)
    val validatorWrongConfig2: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings2), appServerInformationWithoutPrefix)
    val validatorWrongConfig3: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings3), appServerInformationWithoutPrefix)
    val validatorWrongConfig4: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings4), appServerInformationWithoutPrefix)
    val validatorWrongConfig5: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings5), appServerInformationWithoutPrefix)
    val validatorWrongConfig6: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings6), appServerInformationWithoutPrefix)
    val validatorWrongConfig7: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings7), appServerInformationWithoutPrefix)
    val validatorWrongConfig8: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings8), appServerInformationWithoutPrefix)

    val validatorWrongConfig9: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings9), appServerInformationWithoutPrefix)
    val validatorWrongConfig10: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings10), appServerInformationWithoutPrefix)
    val validatorWrongConfig11: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings11), appServerInformationWithoutPrefix)
    val validatorWrongConfig12: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings12), appServerInformationWithoutPrefix)
    val validatorWrongConfig13: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings13), appServerInformationWithoutPrefix)
    val validatorWrongConfig14: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = configurationMishapSettings14), appServerInformationWithoutPrefix)


    val validatorWrongConfig15: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings, null)
    val validatorWrongConfig16: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings, appServerInformationMishap1)
    val validatorWrongConfig17: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings, appServerInformationMishap2)
    val validatorWrongConfig18: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings, appServerInformationMishap3)


    val validatorBlacklist: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBlacklist), appServerInformationWithoutPrefix)
    val validatorBlacklist2: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBlacklist), appServerInformationWithPrefix)
    val validatorBlacklist3: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBlacklist2), appServerInformationWithPrefix)

    val validatorWhitelist: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsWhitelist), appServerInformationWithoutPrefix)
    val validatorWhitelist2: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsWhitelist), appServerInformationWithPrefix)


    val validatorBothList: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBothList), appServerInformationWithoutPrefix)
    val validatorBothList2: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBothList), appServerInformationWithPrefix)
    val validatorBothList3: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBothList2), appServerInformationWithoutPrefix)
    val validatorBothList4: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBothList2), appServerInformationWithPrefix)
    val validatorBothList5: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBothList3), appServerInformationWithoutPrefix)
    val validatorBothList6: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBothList3), appServerInformationWithPrefix)


    val validatorPassed0: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsBlacklist), appServerInformationWithFaultyPrefix)
    val validatorPassed1: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettings1), appServerInformationWithoutPrefix)
    val validatorPassed2: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettings2), appServerInformationWithoutPrefix)
    val validatorPassed3: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettings3), appServerInformationWithoutPrefix)
    val validatorPassed4: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettings4), appServerInformationWithoutPrefix)

    val validatorWrongConfiguration1: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsWrongConfig1), appServerInformationWithoutPrefix)
    val validatorWrongConfiguration2: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings.copy(security = securitySettingsWrongConfig2), appServerInformationWithoutPrefix)

    val requestContextMock: RequestContext = mock[RequestContext]

  }


  trait ExceptionFixtureIPv4 extends BaseFixture {
    val crippleMasterUriIPv4: String = "127.0.0.1:99999"
    val uriWithPortIPv4: Uri = {
      val tempUri: Uri = Uri("").withHost("127.0.0.1").withScheme("http")
      tempUri.copy(authority = tempUri.authority.copy(port = 30))
    }
    val expectedLogIPv4: String = s"""An error: {java.lang.NumberFormatException} occurred during the IPv4-Address validation process of the source: {${uriWithPortIPv4.toString}} against the trusted source: {$crippleMasterUriIPv4}. The reason was: {For input string: "leet"}"""
    val loggerMockIPv4: Logger = mock[Logger]
  }

  trait ExceptionFixture extends BaseFixture {
    val crippleMasterUri: String = "as:33:test"
    val uriWithPort: Uri = {
      val tempUri: Uri = Uri("http://test")
      tempUri.copy(authority = tempUri.authority.copy(port = 33))
    }

    val expectedLog: String = s"""An error: {java.lang.NumberFormatException} occurred during the domain validation process of the source: {${uriWithPort.toString}} against the trusted source: {$crippleMasterUri}. The reason was: {For input string: "test"}"""
    val loggerMock: Logger = mock[Logger]
  }

  trait BaseFixture extends MockitoSugar {
    def DomainWarning: (Throwable, Uri, String) => String = TrustedSourcesValidator.DomainWarning
    def IPv4Warning: (Throwable, Uri, String) => String = TrustedSourcesValidator.IPv4Warning

    val appServerPrefix: String = "mySexyAppServer"
    val appServerInformation: AppServerInformation = AppServerInformation(None, None)

    val pluginDescription: PluginDescription = new PluginDescription("", "", "", "", None)
    val pluginSettings: PluginSettings = PluginSettings("", -1, "", None, "", -1, "", FiniteDuration(10L, TimeUnit.SECONDS), FiniteDuration(10L, TimeUnit.SECONDS), FiniteDuration(10L, TimeUnit.SECONDS),
      Security(IncomingRequests(BlackAndWhiteList(Seq(), Seq()), allowUntrustedSources = true)))
    val validator: TrustedSourcesValidator = new TrustedSourcesValidator(pluginSettings, appServerInformation)

    val isOnlyWildcard1: Seq[String] = Seq("*", "*", "*", "*", "*", "*")
    val isOnlyWildcard2: Seq[String] = Seq("*")
    val isOnlyWildcard3: Seq[String] = Seq()
    val isNotOnlyWildcard1: Seq[String] = Seq("aa", "*")
    val isNotOnlyWildcard2: Seq[String] = Seq("*", "aa", "*")
    val isNotOnlyWildcard3: Seq[String] = Seq("*", "aa", "*", "s")
    val isNotOnlyWildcard4: Seq[String] = Seq("*", "aa", "*", "*")

    val emptyString: String = ""
  }

}
