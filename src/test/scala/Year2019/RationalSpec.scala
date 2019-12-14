package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class RationalSpec extends AnyWordSpec with Matchers {

  "Rational" should {
    "reduction fractions to their lowest term" in {
      Rational(4, 12).lowestTerms shouldBe Rational(1, 3)
    }

    "be able to handle zero divisors" in {
      Rational(2, 0).lowestTerms shouldBe Rational(1, 0)
    }

    "add two rationals" in {
      Rational(1, 2) + Rational(1, 4) shouldBe Rational(3, 4)
    }

    "multiply two rationals" in {
      Rational(1, 2) * Rational(1, 4) shouldBe Rational(1, 8)
    }
  }

}
