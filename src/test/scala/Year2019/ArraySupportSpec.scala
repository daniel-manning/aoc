package Year2019

import models.Position
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ArraySupportSpec extends AnyWordSpec with Matchers {

  "Array Reconstruction" should {
    "take a sequence of elements with position and reconstruct array" in {
      val sequence = Seq((Position(0,0),0),(Position(1,0),2),(Position(0,1),2),(Position(1,1),2))
      val result = Seq(Seq(0,2), Seq(2,2))

      ArraySupport.reconstructArray(sequence) shouldBe result
    }

    "construct position map from array of sequences" in {
      val sequence = Seq(Seq(0,2), Seq(2,2))
      val result = Seq((Position(0,0),0),(Position(1,0),2),(Position(0,1),2),(Position(1,1),2))
      ArraySupport.sequenceArrayToPositionSeq(sequence)(identity) shouldBe result
    }
  }

}
