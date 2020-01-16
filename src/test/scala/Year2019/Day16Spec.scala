package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day16Spec extends AnyWordSpec with Matchers {

  "Sequence Multiplier" should {
    "multiply sequences" in {
      SeqMultiplier.times(Seq(1,2), 3) shouldBe Seq(1,2,1,2,1,2)
    }
  }

  "FFT" should {
    "turn a string of numbers into a sequence" in {
      FFT.stringToSequence("15243") shouldBe Seq(1,5,2,4,3)
    }

    "apply a pattern to a phase" in {
      FFT.applyPatternToPhase(Seq(9,8,7,6,5), Seq(1,2,3)) shouldBe 62
    }

    "keep only the least significant digit for positive numbers" in {
      FFT.keepLeastSignificantDigit(38) shouldBe 8
    }

    "keep only the least significant digit for negative numbers" in {
      FFT.keepLeastSignificantDigit(-17) shouldBe 7
    }
  }

}
