package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day02Spec extends AnyWordSpec with Matchers{


  "A programme" should {
    "edit in place it's code values" in {
      val startingProgramme = IntCodeProgramme(programme = Vector(1,9,10,3,2,3,11,0,99,30,40,50))
      val op = startingProgramme.nextOperation()
      val alteredProgramme = op.run(startingProgramme)(RunningSettings("", debugOutput = false))
      alteredProgramme shouldBe IntCodeProgramme(4, programme = Vector(1,9,10,70,2,3,11,0,99,30,40,50))
    }

    "start at a separate starting place for the next opcode" in {
      val startingProgramme = IntCodeProgramme(4, Vector(1,9,10,70,2,3,11,0,99,30,40,50))
      val op = startingProgramme.nextOperation()
      val alteredProgramme = op.run(startingProgramme)(RunningSettings("", debugOutput = false))
      alteredProgramme shouldBe IntCodeProgramme(8, programme = Vector(3500,9,10,70,2,3,11,0,99,30,40,50))
    }

    "end when it hits the end opcode" in {
      val startingProgramme = IntCodeProgramme(8, Vector(3500,9,10,70,2,3,11,0,99,30,40,50))
      val op = startingProgramme.nextOperation()
      val alteredProgramme = op.run(startingProgramme)(RunningSettings("", debugOutput = false))
      alteredProgramme shouldBe IntCodeProgramme(8, programme = Vector(3500,9,10,70,2,3,11,0,99,30,40,50))
    }
  }

  "Running a programme" should {
    "run correctly through to the end" in {
      val startingProgramme = IntCodeProgramme(programme = Vector(1,9,10,3,2,3,11,0,99,30,40,50))
      val alteredProgramme = startingProgramme.runProgramme()

      alteredProgramme shouldBe IntCodeProgramme(8, programme = Vector(3500,9,10,70,2,3,11,0,99,30,40,50))
    }

    "run an addition correctly" in {
      val startingProgramme = IntCodeProgramme(programme = Vector(1,0,0,0,99))
      val alteredProgramme = startingProgramme.runProgramme()

      alteredProgramme shouldBe IntCodeProgramme(4, programme = Vector(2,0,0,0,99))
    }

    "run a multiplication correctly" in {
      val startingProgramme = IntCodeProgramme(programme = Vector(2,3,0,3,99))
      val alteredProgramme = startingProgramme.runProgramme()

      alteredProgramme shouldBe IntCodeProgramme(4, programme = Vector(2,3,0,6,99))
    }

    "run a multiplication on an opcode" in {
      val startingProgramme = IntCodeProgramme(programme = Vector(2,4,4,5,99,0))
      val alteredProgramme = startingProgramme.runProgramme()

      alteredProgramme shouldBe IntCodeProgramme(4, programme = Vector(2,4,4,5,99,9801))
    }

    "run multiple operations correctly" in {
      val startingProgramme = IntCodeProgramme(programme = Vector(1,1,1,4,99,5,6,0,99))
      val alteredProgramme = startingProgramme.runProgramme()

      alteredProgramme shouldBe IntCodeProgramme(8, programme = Vector(30,1,1,4,2,5,6,0,99))
    }
  }



}
