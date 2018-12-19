package Day09

import org.scalatest.{Matchers, WordSpec}

class Day09Spec extends WordSpec with Matchers{

  "Circular Marble List" should {
    "move two and add the next marble" in {
      val data = CircularMarbleList(0, List(0,2,1,3), 9, 3, List())
      val result = CircularMarbleList(2, List(0,4,2,1,3), 9, 4, List())

      data.addNextMarbleBetweenTheNextTwo() shouldBe result
    }

    "move two and add the next marble at the end of a list" in {
      val data = CircularMarbleList(6, List(0, 4, 2, 5, 1, 6, 3), 9, 6, List())
      val result = CircularMarbleList(0, List(0,4,2,5,1,6,3,7), 9, 7, List())

      data.addNextMarbleBetweenTheNextTwo() shouldBe result
    }

    "evolve from zero" in {
      val result = List(0,1)

      Marbles.evolve(CircularMarbleList(0, List(0), 9, 0, List())).marbleList shouldBe result
    }

    "evolve from one" in {
      val result = CircularMarbleList(2, List(0,2,1), 9, 2, List())

      Marbles.evolve(CircularMarbleList(0, List(0, 1), 9, 1, List())) shouldBe result
    }

    "evolve the pattern correctly over 15 turns" in {
      val result = List(0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15)

      Marbles.runTurns(CircularMarbleList(0, List(0), 9, 0, List()), 15).marbleList shouldBe result
    }
  }


}
