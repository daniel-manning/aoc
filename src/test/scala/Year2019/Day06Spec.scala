package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day06Spec extends AnyWordSpec with Matchers {
  "Construct Orbital Map" should {
    "construct a map from a single orbit in List" in {
      val expectedResult = OrbitalObject("COM", Seq(OrbitalObject("B", Nil)))
      OrbitalObject.constructFromOrbitList(Seq("COM)B")) shouldBe expectedResult
    }

    "construct a map from nested orbit in List" in {
      val expectedResult = OrbitalObject("COM", Seq(OrbitalObject("B", Seq(OrbitalObject("C", Nil)))))
      OrbitalObject.constructFromOrbitList(Seq("COM)B", "B)C")) shouldBe expectedResult
    }
  }



}
