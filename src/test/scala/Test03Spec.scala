import Day03.{Overlap, Rectangle}
import org.scalatest.FeatureSpec

class Test03Spec extends FeatureSpec {

  feature("Rectangle") {
    scenario("finds the correct overlap of rectangles") {
      val rectangles = Seq("#1 @ 1,3: 4x4","#2 @ 3,1: 4x4","#3 @ 5,5: 2x2").map(Rectangle.stringToRectangle)

      assert(Overlap.calculateOverlap(rectangles) == 4)
    }


    scenario("finds the point in a rectangle") {
      val point = (1.5, 3.5)
      val rectangle = Rectangle.stringToRectangle("#1 @ 1,3: 4x4")

      assert(Overlap.pointInRectangle(point, rectangle))
    }

    scenario("finds the point out of a rectangle") {
      val point = (0.5, 3.5)
      val rectangle = Rectangle.stringToRectangle("#1 @ 1,3: 4x4")

      assert(!Overlap.pointInRectangle(point, rectangle))
    }

  }


}