package Year2018

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day03Spec extends AnyWordSpec with Matchers {

  "Rectangle" ignore {


    "intersect two seperate rectangles with nothing" in {
      val rectangleOne = Rectangle.stringToRectangle("#1 @ 1,3: 4x4")
      val rectangleTwo = Rectangle.stringToRectangle("#3 @ 5,5: 2x2")

      rectangleOne.intersect(rectangleTwo) shouldBe None
    }

    "intersect two overlapping rectangles" in {
      val rectangleOne = Rectangle.stringToRectangle("#1 @ 1,1: 4x4")
      val rectangleTwo = Rectangle.stringToRectangle("#3 @ 3,3: 6x6")

      val resultRectangle = Rectangle("", (3,3), 2, 2)

      rectangleOne.intersect(rectangleTwo) shouldBe Some(resultRectangle)
    }


    "intersect two overlapping rectangles with one within" in {
      val rectangleOne = Rectangle.stringToRectangle("#1 @ 1,1: 7x7")
      val rectangleTwo = Rectangle.stringToRectangle("#2 @ 3,3: 2x2")

      val resultRectangle = Rectangle("", (3,3), 2, 2)

      rectangleOne.intersect(rectangleTwo) shouldBe Some(resultRectangle)
    }

   "finds the correct overlap of rectangles" in {
      val rectangles = List("#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2").map(Rectangle.stringToRectangle)

      Overlap.calculateOverlap(rectangles) shouldBe 4
    }

  }


}