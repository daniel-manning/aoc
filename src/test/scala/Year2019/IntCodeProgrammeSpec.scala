package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import ProgrammeOperations._

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
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(1002,4,3,4,33)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.programme shouldBe vectorProgrammeToMap(Vector(1002,4,3,4,99))
    }
  }

  "position mode equal to" should {
    "compare input to 8 and output 1 for true" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,9,8,9,10,9,4,9,99,-1,8)),
        inputQueue = new mutable.Queue().enqueue(BigInt(8)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1)
    }

    "compare input to 8 and output 0 for false" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,9,8,9,10,9,4,9,99,-1,8)),
        inputQueue = new mutable.Queue().enqueue(BigInt(7)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(0)

    }
  }

  "position mode less than" should {
    "compare input to 8 and output 1 for true" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,9,7,9,10,9,4,9,99,-1,8)),
        inputQueue = new mutable.Queue().enqueue(BigInt(7)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1)
    }

    "compare input to 8 and output 0 for false" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,9,7,9,10,9,4,9,99,-1,8)),
        inputQueue = new mutable.Queue().enqueue(BigInt(10)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(0)
    }
  }

  "immediate mode equal to" should {
    "compare input to 8 and output 1 for true" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,3,1108,-1,8,3,4,3,99)),
        inputQueue = new mutable.Queue().enqueue(BigInt(8)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1)
    }

    "compare input to 8 and output 0 for false" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,3,1108,-1,8,3,4,3,99)),
        inputQueue = new mutable.Queue().enqueue(BigInt(7)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(0)
    }
  }

  "immediate mode less than" should {
    "compare input to 8 and output 1 for true" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,3,1107,-1,8,3,4,3,99)),
        inputQueue = new mutable.Queue().enqueue(BigInt(7)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1)
    }

    "compare input to 8 and output 0 for false" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,3,1107,-1,8,3,4,3,99)),
        inputQueue = new mutable.Queue().enqueue(BigInt(10)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(0)
    }
  }

  "position mode jump test" should {
    "output zero if input is zero" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,12,6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)),
        inputQueue = new mutable.Queue().enqueue(BigInt(0)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(0)
    }

    "output 1 if greater than zero" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9)),
        inputQueue = new mutable.Queue().enqueue(BigInt(10)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1)
    }
  }

  "immediate mode jump test" should {
    "output zero if input is zero" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)),
        inputQueue = new mutable.Queue().enqueue(BigInt(0)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(0)
    }

    "output 1 if greater than zero" in {
      val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(Vector(3,3,1105,-1,9,1101,0,0,12,4,12,99,1)),
        inputQueue = new mutable.Queue().enqueue(BigInt(10)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1)
    }
  }

  "large test case" should {
    val programme = vectorProgrammeToMap(Vector(3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
    1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
    999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99))

    "output 999 if the input value is below 8" in {
      val startProgramme = IntCodeProgramme(programme = programme,
        inputQueue = new mutable.Queue().enqueue(BigInt(1)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(999)
    }

    "output 1000 if the input value is equal to 8" in {
      val startProgramme = IntCodeProgramme(programme = programme,
        inputQueue = new mutable.Queue().enqueue(BigInt(8)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1000)
    }

    "output 1001 if the input value is greater than 8" in {
      val startProgramme = IntCodeProgramme(programme = programme,
        inputQueue = new mutable.Queue().enqueue(BigInt(9)))
      val endProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(1001)
    }
  }

  "relative base adjustment" should {
    "adjust the relative base" in {
      val programme = vectorProgrammeToMap(Vector(109,19,204,-34, 99))
      val startProgramme = IntCodeProgramme(relativeBase = 2000, programme = programme)
      val endProgramme: IntCodeProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(0)
    }

    "allow creation of a quine" in {
      val programme = vectorProgrammeToMap(Vector(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
      val startProgramme = IntCodeProgramme(programme = programme)
      val endProgramme: IntCodeProgramme = startProgramme.runProgramme()

      endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    }

    "Int codes" should {
      "allow for the creation of large numbers" in {
        val programme = vectorProgrammeToMap(Vector(1102,34915192,34915192,7,4,7,99,0))
        val startProgramme = IntCodeProgramme(programme = programme)
        val endProgramme: IntCodeProgramme = startProgramme.runProgramme()

        endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(BigInt("1219070632396864"))
      }

      "allow for large programme codes" in {
        val programme = vectorProgrammeToMap(Vector(BigInt(104),BigInt("1125899906842624"),BigInt(99)))
        val startProgramme = IntCodeProgramme(programme = programme)
        val endProgramme: IntCodeProgramme = startProgramme.runProgramme()

        endProgramme.outputQueue.dequeueAll(_ => true) shouldBe Seq(BigInt("1125899906842624"))

      }
    }


  }

}
