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

  "foldUpRuns" should {
    "create a list of tuples when there are no runs" in {
      Day04.foldUpRuns(Seq(3,4,5,7,9)) shouldBe Seq((3,1),(4,1),(5,1),(7,1),(9,1))
    }

    "create a list of tuples when there are runs" in {
      Day04.foldUpRuns(Seq(3,4,4,4,5,5,9,7,7)) shouldBe Seq((3,1),(4,3),(5,2),(9,1),(7,2))
    }
  }

  "new criteria" should {
    "pass in test case one" in {
      Day04.hasTwoAdjacentNumbersTheSameNotALongerRun(Day04.constructSequence(112233)) shouldBe true
    }

    "fail in test case two" in {
      Day04.hasTwoAdjacentNumbersTheSameNotALongerRun(Day04.constructSequence(123444)) shouldBe false
    }

    "pass in test case three" in {
      Day04.hasTwoAdjacentNumbersTheSameNotALongerRun(Day04.constructSequence(111122)) shouldBe true
    }
  }

}
