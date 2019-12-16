package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SeqOpsSupport extends AnyWordSpec with Matchers {

  "Sequences" should {
    "be able to cross with different pairs" in {
      val result = Seq((1,2), (1, 3), (2, 1), (2, 3))
      SeqOps.crossDifferentPairs(Seq(1,2,3)) shouldBe result
    }
  }
}
