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

/*
    "apply a pattern to a phase" in {
      FFT.applyPatternToPhase(Seq(9,8,7,6,5), Seq(1,2,3)) shouldBe 62
    }
*/

    "keep only the least significant digit for positive numbers" in {
      FFT.keepLeastSignificantDigit(38) shouldBe 8
    }

    "keep only the least significant digit for negative numbers" in {
      FFT.keepLeastSignificantDigit(-17) shouldBe 7
    }

    "use the correct pattern for digits used" in {
      FFT.considerPatternForDigit(Vector(0,1,0,-1), 3) shouldBe Vector(0,0,0,1,1,1,0,0,0,-1,-1,-1)
    }

    "correctly apply phase 1 for test case 1" in {
      FFT.applyPhase(Vector(1,2,3,4,5,6,7,8)) shouldBe Vector(4,8,2,2,6,1,5,8)
    }

    "correctly apply phase 2 for test case 1" in {
      FFT.applyPhase(Vector(4,8,2,2,6,1,5,8)) shouldBe Vector(3,4,0,4,0,4,3,8)
    }

    "correctly apply phase 3 for test case 1" in {
      FFT.applyPhase(Vector(3,4,0,4,0,4,3,8)) shouldBe Vector(0,3,4,1,5,5,1,8)
    }

    "correctly apply phase 4 for test case 1" in {
      FFT.applyPhase(Vector(0,3,4,1,5,5,1,8)) shouldBe Vector(0,1,0,2,9,4,9,8)
    }

    "After 4 phases have the correct output for test case 1" in {
      FFT.applyForPhases(Vector(1,2,3,4,5,6,7,8), 4) shouldBe Vector(0,1,0,2,9,4,9,8)
    }

    "Apply 100 phases correctly for test case 2" in {
      FFT.apply100PhasesAndKeep8MostSignificant(FFT.stringToSequence("80871224585914546619083218645595")) shouldBe
        FFT.stringToSequence("24176176")
    }

    "Apply 100 phases correctly for test case 3" in {
      FFT.apply100PhasesAndKeep8MostSignificant(FFT.stringToSequence("19617804207202209144916044189917")) shouldBe
        FFT.stringToSequence("73745418")
    }

    "Apply 100 phases correctly for test case 4" in {
      FFT.apply100PhasesAndKeep8MostSignificant(FFT.stringToSequence("69317163492948606335995924319873")) shouldBe
        FFT.stringToSequence("52432133")
    }
  }

}
