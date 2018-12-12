package Day06

import org.scalatest.{Matchers, WordSpec}

class Day06Spec extends WordSpec with Matchers {

  "Distance" should {
    "find correct manhattan distance between two points" in {
      val pointOne = (1, 1)
      val pointTwo = (4, 5)

      Distance.manhattanDistance(pointOne, pointTwo) shouldBe 7
    }

    "generate grid should make a list of points" in {
      val grid = List((0,0), (0,1), (0,2), (1,0), (1,1), (1,2), (2,0), (2,1), (2,2))

      Distance.generateGrid(3,3) shouldBe grid
    }

    "calculate claimed areas" in { 
      val targets = List((1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9))
      val answers = List((1,), (2,), (3,), (4,), (5,), (6,))

      Distance.calculateAreas(targets)


    }
  }
}
