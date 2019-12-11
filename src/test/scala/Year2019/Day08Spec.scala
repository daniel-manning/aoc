package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day08Spec extends AnyWordSpec with Matchers {

  "SpaceImage Format" should {
    "be constructed from a sequence of pixels with a dimension" in {
      val sequence = Seq(1,2,3,4,5,6,7,8,9,0,1,2)
      val layer1 = Seq(Seq(1,2,3), Seq(4,5,6))
      val layer2 = Seq(Seq(7,8,9), Seq(0,1,2))
      val result = SpaceImageFormat(Seq(SpaceImageLayer(layer1), SpaceImageLayer(layer2)))

      SpaceImageFormat.constructLayeredImage(sequence, (3, 2)) shouldBe result
    }

    "find the layer with the fewest zeros" in {
      val sequence = Seq(1,2,3,4,5,6,7,8,9,0,1,2)
      val image = SpaceImageFormat.constructLayeredImage(sequence, (3, 2))
      val layer1 = Seq(Seq(1,2,3), Seq(4,5,6))

      image.layerWithTheFewestZeros shouldBe SpaceImageLayer(layer1)
    }

    "render and image correctly" in {
      val result = RenderedImage(Seq(Seq(Black, White), Seq(White, Black)))
      val sequence = Seq(0,2,2,2,1,1,2,2,2,2,1,2,0,0,0,0)
      val image = SpaceImageFormat.constructLayeredImage(sequence, (2, 2))
      image.renderImage shouldBe result
    }
  }

}
