package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day04Spec extends AnyWordSpec with Matchers {

  "isMonatonicSequence" should {
    "be true for monatonic sequence" in {
      Day04.isMonotonicSequence(Seq(1,2,3,3,3,4,4,5,9)) shouldBe true
    }

    "be false for non-monatonic sequence" in {
      Day04.isMonotonicSequence(Seq(1,2,3,7,3,4,4,5,9)) shouldBe false
    }
  }

  "constructSequence" should {
    "turn a comma separated list into a list of numbers" in {
      Day04.constructSequence("1,2,3,4,5,7") shouldBe Seq(1,2,3,4,5,7)
    }

    "turn number string into sequence" in {
      Day04.constructSequence("4,9,7") shouldBe Seq(4,9,7)
    }

    "turn number into sequence" in {
      Day04.constructSequence(497) shouldBe Seq(4,9,7)
    }
  }

  "hasTwoAdjacentNumbersTheSame" should {
    "return true for two adjacent numbers the same" in {
      Day04.hasTwoAdjacentNumbersTheSame(Seq(3,4,5,5,9)) shouldBe true
    }

    "return false for no two adjacent numbers the same" in {
      Day04.hasTwoAdjacentNumbersTheSame(Seq(3,4,5,7,9)) shouldBe false
    }
  }

}
