package io.simplifier.pluginbase.util.technicalname

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.{Logger, LoggerFactory}

class TechnicalNameReducerTest extends AnyWordSpec with Matchers with TechnicalNameReducer {

  "The Technical Name Reducer" should {
    "keep alphanumerical chars" in {
      val safeInput1 = "ValidName1"
      val output1 = TechnicalNameReducer.reduceToTechnicalName(safeInput1)
      output1 mustBe safeInput1

      val safeInput2 = "this_IS_al50_v4liD"
      val output2 = TechnicalNameReducer.reduceToTechnicalName(safeInput2)
      output2 mustBe safeInput2
    }
    "strip invalid chars" in {
      val unsafeInput = "@Stip ~This&%Doöwn:::tøo b4s!ic_"
      val expectedOutput = "StipThisDowntob4sic_"
      val output = TechnicalNameReducer.reduceToTechnicalName(unsafeInput)
      output mustBe expectedOutput
    }
    "prefix names starting with a dgit" in {
      val unsafeInput = "2_de"
      val expectedOutput = "_2_de"
      val output = reduceToTechnicalName(unsafeInput,addPrefix = true)
      output mustBe expectedOutput
    }
  }
  override val Logger: Logger = LoggerFactory.getLogger(getClass.getName.stripSuffix("$"))
}
