package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Spec extends AnyWordSpec with Matchers {

  "AsteroidField" should {
    "construct from String" in {
      val fieldString =
        """.#..#
          |.....
          |#####
          |....#
          |...##""".stripMargin
      val result = Map(
        Position(0,0) -> Empty, Position(1,0) -> Asteroid, Position(2,0) -> Empty, Position(3,0) -> Empty, Position(4,0) -> Asteroid,
        Position(0,1) -> Empty, Position(1,1) -> Empty, Position(2,1) -> Empty, Position(3,1) -> Empty, Position(4,1) -> Empty,
        Position(0,2) -> Asteroid, Position(1,2) -> Asteroid, Position(2,2) -> Asteroid, Position(3,2) -> Asteroid, Position(4,2) -> Asteroid,
        Position(0,3) -> Empty, Position(1,3) -> Empty, Position(2,3) -> Empty, Position(3,3) -> Empty, Position(4,3) -> Asteroid,
        Position(0,4) -> Empty, Position(1,4) -> Empty, Position(2,4) -> Empty, Position(3,4) -> Asteroid, Position(4,4) -> Asteroid
      )

      AsteroidField.construct(fieldString.split("\n")) shouldBe AsteroidField(result)
    }
  }
}
