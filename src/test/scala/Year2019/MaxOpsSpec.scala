package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.HashMap

class MaxOpsSpec extends AnyWordSpec with Matchers {
        "MaxBy" should {
          "find a single max element where unique" in {
            MaxOps.maxBy(List(1,2,3))(identity) shouldBe Seq(3)
          }

          "find multiple elements where they share a maximum" in {
            MaxOps.maxBy(List(1,2,3,3,1,3))(identity) shouldBe Seq(3,3,3)
          }

          "find multiple in test case" in {
            val map = HashMap(OrbitalObject("G") -> 2,
              OrbitalObject("F") -> 5,
              OrbitalObject("D") -> 3,
              OrbitalObject("E") -> 4,
              OrbitalObject("I") -> 4,
              OrbitalObject("COM") -> 0,
              OrbitalObject("J") -> 5,
              OrbitalObject("C") -> 2,
              OrbitalObject("B") -> 1)

            MaxOps.maxBy(map)(_._2) shouldBe Seq(OrbitalObject("F") -> 5, OrbitalObject("J") -> 5)
          }
        }

}
