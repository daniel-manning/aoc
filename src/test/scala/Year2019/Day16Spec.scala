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

    "use the correct pattern for digits used" in {
      FFT.considerPatternForDigit(Seq(0,1,0,-1), 3) shouldBe Seq(0,0,0,1,1,1,0,0,0,-1,-1,-1)
    }

    "correctly apply phase 1 for test case 1" in {
      FFT.applyPhase(Seq(1,2,3,4,5,6,7,8)) shouldBe Seq(4,8,2,2,6,1,5,8)
    }

    "correctly apply phase 2 for test case 1" in {
      FFT.applyPhase(Seq(4,8,2,2,6,1,5,8)) shouldBe Seq(3,4,0,4,0,4,3,8)
    }

    "correctly apply phase 3 for test case 1" in {
      FFT.applyPhase(Seq(3,4,0,4,0,4,3,8)) shouldBe Seq(0,3,4,1,5,5,1,8)
    }

    "correctly apply phase 4 for test case 1" in {
      FFT.applyPhase(Seq(0,3,4,1,5,5,1,8)) shouldBe Seq(0,1,0,2,9,4,9,8)
    }
  }

}
