package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day06Spec extends AnyWordSpec with Matchers {

  val testOrbitList = List("COM)B","B)C","C)D","D)E","E)F","B)G","G)H","D)I","E)J","J)K","K)L")
  val largestMap = OrbitMap(Map(
    OrbitalObject("COM") -> Set(OrbitalObject("B")),
    OrbitalObject("B") -> Set(OrbitalObject("C"), OrbitalObject("G")),
    OrbitalObject("C") -> Set(OrbitalObject("D")),
    OrbitalObject("D") -> Set(OrbitalObject("E"), OrbitalObject("I")),
    OrbitalObject("E") -> Set(OrbitalObject("F"), OrbitalObject("J")),
    OrbitalObject("G") -> Set(OrbitalObject("H")),
    OrbitalObject("J") -> Set(OrbitalObject("K")),
    OrbitalObject("K") -> Set(OrbitalObject("L"))
  ))

  val testSantaOrbit = testOrbitList ++ List("K)YOU", "I)SAN")

  "Construct Orbital Map" should {
    "construct a map from a single orbit in List" in {
      val expectedResult = OrbitMap(Map(OrbitalObject("COM") -> Set(OrbitalObject("B"))))
      OrbitMap.constructFromOrbitList(Seq("COM)B")) shouldBe expectedResult
    }

    "construct a map from nested orbit in List" in {
      val expectedResult = OrbitMap(Map(OrbitalObject("COM") -> Set(OrbitalObject("B")), OrbitalObject("B") -> Set(OrbitalObject("C"))))
      OrbitMap.constructFromOrbitList(Seq("COM)B", "B)C")) shouldBe expectedResult
    }

    "construct the map from the full orbit list" in {
      OrbitMap.constructFromOrbitList(testOrbitList) shouldBe largestMap
    }
  }

  "Distance Map" should {
    "calculate direct and indirect orbit distances" in {
      OrbitMap.constructFromOrbitList(Seq("COM)B"))
        .createDistanceMap() shouldBe Map(OrbitalObject("COM") -> 0, OrbitalObject("B") -> 1)
    }

    "calculate indirect orbit for D" in {
      OrbitMap.constructFromOrbitList(testOrbitList).createDistanceMap()(OrbitalObject("D")) shouldBe 3
    }

    "calculate indirect orbit for L" in {
      OrbitMap.constructFromOrbitList(testOrbitList).createDistanceMap()(OrbitalObject("L")) shouldBe 7
    }
  }

  "Totalling orbits" should {
    "calculate the correct value for test case" in {
      OrbitMap.constructFromOrbitList(testOrbitList).totalOrbits shouldBe 42
    }
  }

  "Find Path" should {
    "find the path to the start" in {
      OrbitMap.constructFromOrbitList(testOrbitList).findPathToStartNode(OrbitalObject("K")) shouldBe
        Seq(OrbitalObject("K"), OrbitalObject("J"), OrbitalObject("E"), OrbitalObject("D"), OrbitalObject("C"),
          OrbitalObject("B"), OrbitalObject("COM"))
    }

    "find path between YOU and SAN" in {
      OrbitMap.constructFromOrbitList(testSantaOrbit).findPathBetweenTwoNodes(OrbitalObject("YOU"), OrbitalObject("SAN")) shouldBe
        Seq(OrbitalObject("YOU"),OrbitalObject("K"),OrbitalObject("J"),OrbitalObject("E"),OrbitalObject("D"),OrbitalObject("I"),OrbitalObject("SAN"))
    }

    "total the distance between nodes to hop correctly" in {
      OrbitMap.constructFromOrbitList(testSantaOrbit).totalNumberOfGravityHopsBetweenTwoOrbitals(OrbitalObject("YOU"), OrbitalObject("SAN")) shouldBe 4
    }
  }


}
