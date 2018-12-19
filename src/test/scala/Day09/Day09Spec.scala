package Day09

import org.scalatest.{Matchers, WordSpec}

class Day09Spec extends WordSpec with Matchers{

  "Circular Marble List" should {
    "move two and add the next marble" in {
      val data = CircularMarbleList(0, List(0,2,1,3))
      val result = CircularMarbleList(2, List(0,4,2,1,3))

      data.addNextMarbleBetweenTheNextTwo() shouldBe result
    }

    "move two and add the next marble at the end of a list" in {
      val data = CircularMarbleList(6, List(0, 4, 2, 5, 1, 6, 3))
      val result = CircularMarbleList(2, List(0,4,2,5,1,6,3,7))

      data.addNextMarbleBetweenTheNextTwo() shouldBe result
    }
  }


}
