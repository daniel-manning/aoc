package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TwoVectorSpec extends AnyWordSpec with Matchers {

  "Two vectors" should {
    "calculate a zero for orthogonal vectors" in {
      TwoVector(1, 0).dotProduct(TwoVector(0, 1)) shouldBe 0
    }

    "calculate norm" in {
      TwoVector(3, 4).norm shouldBe 5
    }
  }

}
