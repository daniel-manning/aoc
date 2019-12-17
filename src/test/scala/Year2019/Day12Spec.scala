package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day12Spec extends AnyWordSpec with Matchers {
  "ThreeVector Parser" should {
    "correctly parse strings" in {
      val input =
        """<x=-1, y=0, z=2>
          |<x=2, y=-10, z=-7>
          |<x=4, y=-8, z=8>
          |<x=3, y=5, z=-1>""".stripMargin

      val result = Seq(
        ThreeVector(-1, 0, 2),
        ThreeVector(2, -10, -7),
        ThreeVector(4, -8, 8),
        ThreeVector(3, 5, -1)
      )

      input
        .split("\n").toList
        .map(s => ThreeVectorParser.parse(ThreeVectorParser.tvp, s).get) shouldBe result
    }
  }

  "Apply Gravity" should {
    "consider gravity correctly" in {
      val ganymede = SystemBody("Ganymede", ThreeVector(3, 2, 0))
      val callisto = SystemBody("Callisto", ThreeVector(5, 1, 0))

      SystemBody.considerPairs(ganymede, callisto) shouldBe
        (("Ganymede", ThreeVector(1, -1, 0)),
        ("Callisto", ThreeVector(-1, 1, 0)))
    }

    "move body correctly" in {
      val europa = SystemBody("Europa", ThreeVector(1, 2, 3), ThreeVector(-2, 0, 3))
      val europaUpdated = SystemBody("Europa", ThreeVector(-1, 2, 6), ThreeVector(-2, 0, 3))

      europa.moveBody shouldBe europaUpdated
    }

    "evolve System one step" in {
      val system = NBodySystem(Seq(
        SystemBody("a", ThreeVector(-1, 0, 2)),
        SystemBody("b", ThreeVector(2, -10, -7)),
        SystemBody("c", ThreeVector(4, -8, 8)),
        SystemBody("d", ThreeVector(3, 5, -1))
      ))

      val result = NBodySystem(Seq(
        SystemBody("a", ThreeVector(2, -1, 1), ThreeVector(3, -1, -1)),
        SystemBody("b", ThreeVector(3, -7, -4), ThreeVector(1, 3, 3)),
        SystemBody("c", ThreeVector(1, -7, 5), ThreeVector(-3, 1, -3)),
        SystemBody("d", ThreeVector(2, 2, 0), ThreeVector(-1, -3, 1))
      ), timeStep = 1)

      NBodySystem.evolveUntilTime(system, 1) shouldBe result
    }

    "evolve System two steps" in {
      val system = NBodySystem(Seq(
        SystemBody("a", ThreeVector(-1, 0, 2)),
        SystemBody("b", ThreeVector(2, -10, -7)),
        SystemBody("c", ThreeVector(4, -8, 8)),
        SystemBody("d", ThreeVector(3, 5, -1))
      ))

      val result = NBodySystem(Seq(
        SystemBody("a", ThreeVector(5, -3, -1), ThreeVector(3, -2, -2)),
        SystemBody("b", ThreeVector(1, -2, 2), ThreeVector(-2, 5, 6)),
        SystemBody("c", ThreeVector(1, -4, -1), ThreeVector(0, 3, -6)),
        SystemBody("d", ThreeVector(1, -4, 2), ThreeVector(-1, -6, 2))
      ), timeStep = 2)

      NBodySystem.evolveUntilTime(system, 2) shouldBe result
    }

    "evolve System three steps" in {
      val system = NBodySystem(Seq(
        SystemBody("a", ThreeVector(-1, 0, 2)),
        SystemBody("b", ThreeVector(2, -10, -7)),
        SystemBody("c", ThreeVector(4, -8, 8)),
        SystemBody("d", ThreeVector(3, 5, -1))
      ))

      val result = NBodySystem(Seq(
        SystemBody("a", ThreeVector(5, -6, -1), ThreeVector(0, -3, 0)),
        SystemBody("b", ThreeVector(0, 0, 6), ThreeVector(-1, 2, 4)),
        SystemBody("c", ThreeVector(2, 1, -5), ThreeVector(1, 5, -4)),
        SystemBody("d", ThreeVector(1, -8, 2), ThreeVector(0, -4, 0))
      ), timeStep = 3)

      NBodySystem.evolveUntilTime(system, 3) shouldBe result
    }

    "evolve System four steps" in {
      val system = NBodySystem(Seq(
        SystemBody("a", ThreeVector(-1, 0, 2)),
        SystemBody("b", ThreeVector(2, -10, -7)),
        SystemBody("c", ThreeVector(4, -8, 8)),
        SystemBody("d", ThreeVector(3, 5, -1))
      ))

      val result = NBodySystem(Seq(
        SystemBody("a", ThreeVector(2, -8, 0), ThreeVector(-3, -2, 1)),
        SystemBody("b", ThreeVector(2, 1, 7), ThreeVector(2, 1, 1)),
        SystemBody("c", ThreeVector(2, 3, -6), ThreeVector(0, 2, -1)),
        SystemBody("d", ThreeVector(2, -9, 1), ThreeVector(1, -1, -1))
      ), timeStep = 4)

      NBodySystem.evolveUntilTime(system, 4) shouldBe result
    }

    "evolve System five steps" in {
      val system = NBodySystem(Seq(
        SystemBody("a", ThreeVector(-1, 0, 2)),
        SystemBody("b", ThreeVector(2, -10, -7)),
        SystemBody("c", ThreeVector(4, -8, 8)),
        SystemBody("d", ThreeVector(3, 5, -1))
      ))

      val result = NBodySystem(Seq(
        SystemBody("a", ThreeVector(-1, -9, 2), ThreeVector(-3, -1, 2)),
        SystemBody("b", ThreeVector(4, 1, 5),   ThreeVector(2, 0, -2)),
        SystemBody("c", ThreeVector(2, 2, -4),  ThreeVector(0, -1, 2)),
        SystemBody("d", ThreeVector(3, -7, -1), ThreeVector(1, 2, -2))
      ), timeStep = 5)

      NBodySystem.evolveUntilTime(system, 5) shouldBe result
    }

    "have the correct kinetic and potential energies after 10 steps" in {
      val system = NBodySystem(Seq(
        SystemBody("a", ThreeVector(-1, 0, 2)),
        SystemBody("b", ThreeVector(2, -10, -7)),
        SystemBody("c", ThreeVector(4, -8, 8)),
        SystemBody("d", ThreeVector(3, 5, -1))
      ))

      NBodySystem
        .evolveUntilTime(system, 5)
        .system
        .map(b => (b.potentialEnergy, b.kineticEnergy)) shouldBe Seq((6,6),(9,5),(10,8),(6, 3))
    }
  }
}
