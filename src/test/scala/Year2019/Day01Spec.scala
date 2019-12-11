package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day01Spec extends AnyWordSpec with Matchers{

  "Fuel Mass Calculated for Module mass" should {
    "calculate for 12" in {
      Year2019.Day01.calculateFuelForModuleMass(12) shouldBe 2
    }

    "calculate for 14" in {
      Year2019.Day01.calculateFuelForModuleMass(14) shouldBe 2
    }

    "calculate for 1969" in {
      Year2019.Day01.calculateFuelForModuleMass(1969) shouldBe 654
    }

    "calculate for 100756" in {
      Year2019.Day01.calculateFuelForModuleMass(100756) shouldBe 33583
    }
  }

  "Calculating the total fuel all in" should {
    "calculate for 1969" in {
      Year2019.Day01.totalFuelAllIn(1969) shouldBe 966
    }

    "calculate for 100756" in {
      Year2019.Day01.totalFuelAllIn(100756) shouldBe 50346
    }
  }

}
