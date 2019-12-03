package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day03Spec extends AnyWordSpec with Matchers{

  "A line" should {
     "be extended by a direction code" in {
        val newLine = Line(Seq((0,0))).add("R8")

       newLine.points.last should be (8, 0)
     }

    "be constructed from a list of commands" in {
       val line = Line.constructFromDirections("R8,U5,L5,D3")
      line.points shouldBe
                    Seq((0,0),
                        (1,0),(2,0),(3,0),(4,0),(5,0),(6,0),(7,0),(8,0),
                        (8,1),(8,2),(8,3),(8,4),(8,5),
                        (7,5),(6,5),(5,5),(4,5),(3,5),
                        (3,4),(3,3),(3,2))
    }
  }

  "We" should {
    "be able to find the intersection of two lines" in {
      val lineOne = Line.constructFromDirections("R8,U5,L5,D3")
      val lineTwo = Line.constructFromDirections("U7,R6,D4,L4")

      Day03.intersectionPointsOfTwoLines(lineOne, lineTwo) shouldBe 6
    }

    "calculate specific examples exactly" in {
      val lineOne = Line.constructFromDirections("R75,D30,R83,U83,L12,D49,R71,U7,L72")
      val lineTwo = Line.constructFromDirections("U62,R66,U55,R34,D71,R55,D58,R83")

      Day03.intersectionPointsOfTwoLines(lineOne, lineTwo) shouldBe 159
    }

    "calculate more specific examples exactly" in {
      val lineOne = Line.constructFromDirections("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
      val lineTwo = Line.constructFromDirections("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

      Day03.intersectionPointsOfTwoLines(lineOne, lineTwo) shouldBe 135
    }
  }

  "intersectionPointsOfTwoLinesConsideringTiming" should {
    "choose the crossing with the least timing" in {
      val lineOne = Line.constructFromDirections("R8,U5,L5,D3")
      val lineTwo = Line.constructFromDirections("U7,R6,D4,L4")

      Day03.intersectionPointsOfTwoLinesConsideringTiming(lineOne, lineTwo) shouldBe 30
    }

    "calculate specific examples exactly" in {
      val lineOne = Line.constructFromDirections("R75,D30,R83,U83,L12,D49,R71,U7,L72")
      val lineTwo = Line.constructFromDirections("U62,R66,U55,R34,D71,R55,D58,R83")

      Day03.intersectionPointsOfTwoLinesConsideringTiming(lineOne, lineTwo) shouldBe 610
    }

    "calculate more specific examples exactly" in {
      val lineOne = Line.constructFromDirections("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51")
      val lineTwo = Line.constructFromDirections("U98,R91,D20,R16,D67,R40,U7,R15,U6,R7")

      Day03.intersectionPointsOfTwoLinesConsideringTiming(lineOne, lineTwo) shouldBe 410
    }
  }

}
