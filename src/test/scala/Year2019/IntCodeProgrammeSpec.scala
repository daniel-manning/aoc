package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class IntCodeProgrammeSpec extends AnyWordSpec with Matchers {

  "parseOpCode" should {
    "correctly parse a full opcode with modes" in {
      IntCodeProgramme.parseOpCode(1002) shouldBe ("02", Seq(PositionMode, ImmediateMode, PositionMode))
    }

    "correctly parse a full opcode without modes" in {
      IntCodeProgramme.parseOpCode(2) shouldBe ("02", Seq(PositionMode, PositionMode, PositionMode))
    }

    "correctly parse a full opcode with modes in RTL" in {
      IntCodeProgramme.parseOpCode(102) shouldBe ("02", Seq(ImmediateMode, PositionMode, PositionMode))
    }
  }

  "mode masks" should {
    "correctly interpret modes in operations" in {
      val startProgramme = IntCodeProgramme(programme = Vector(1002,4,3,4,33))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.programme shouldBe Vector(1002,4,3,4,99)
    }
  }

}
