package Year2018

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class Day09Spec extends AnyWordSpec with Matchers {

  "Circular Marble List" should {
    "move two and add the next marble" in {
      val data = CircularMarbleList(0, List(0,2,1,3), 9, 3, 4, List())
      val result = CircularMarbleList(2, List(0,4,2,1,3), 9, 4, 5, List())

      data.addNextMarbleBetweenTheNextTwo() shouldBe result
    }

    "move two and add the next marble at the end of a list" in {
      val data = CircularMarbleList(6, List(0, 4, 2, 5, 1, 6, 3), 9, 6, 7, List())
      val result = CircularMarbleList(0, List(0,4,2,5,1,6,3,7), 9, 7, 8, List())

      data.addNextMarbleBetweenTheNextTwo() shouldBe result
    }

    "evolve from zero" in {
      val result = List(0,1)

      Marbles.evolve(CircularMarbleList(0, List(0), 9, 0, 1, List())).marbleList shouldBe result
    }

    "evolve from one" in {
      val result = CircularMarbleList(2, List(0,2,1), 9, 2, 3, List())

      Marbles.evolve(CircularMarbleList(0, List(0, 1), 9, 1, 2, List())) shouldBe result
    }

    "evolve the pattern correctly over 15 turns" in {
      val result = List(0, 8, 4, 9, 2, 10, 5, 11, 1, 12, 6, 13, 3, 14, 7, 15)

      Marbles.runTurns(CircularMarbleList(0, List(0), 9, 0, 1, List()), 15).marbleList shouldBe result
    }

    "keep the 23 multiple marble take the marble 7 places back and then move on one" in {
      val setup = CircularMarbleList(14, List(0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 9, 5, 23, List())
      val result = CircularMarbleList(7, List(0, 16, 8, 17, 4, 18, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 9, 6, 24, List((5, 32)))

      Marbles.evolve(setup) shouldBe result
    }

    "evolve after the 23 step" in {
      val setup  = CircularMarbleList(7,  List(0, 16, 8, 17, 4, 18, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 9, 6, 24, List((5, 32)))
      val result = CircularMarbleList(9, List(0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 9, 7, 25, List((5, 32)))

      Marbles.evolve(setup) shouldBe result
    }

    "evolve the pattern including a multiple of 23" in {
      val result = CircularMarbleList(11, List(0, 16, 8, 17, 4, 18, 19, 2, 24, 20, 25, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 9, 8, 26, List((5, 32)))
      val setup  = CircularMarbleList(14, List(0, 16, 8, 17, 4, 18, 9, 19, 2, 20, 10, 21, 5, 22, 11, 1, 12, 6, 13, 3, 14, 7, 15), 9, 5, 23, List())


      Marbles.runTurns(setup, 3) shouldBe result
    }

    "calculte the highscore of a limited game" in {
      Marbles.runGame(9, 25) shouldBe 32
    }

    "calculte the highscore of example limited games" ignore {
      Marbles.runGame(10, 1618) shouldBe 8317
      /*Marbles.runGame(13, 7999) shouldBe 146373
      Marbles.runGame(17, 1104) shouldBe 2764
      Marbles.runGame(21, 6111) shouldBe 54718
      Marbles.runGame(30, 5807) shouldBe 37305*/

    }

  }
}
