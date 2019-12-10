package Year2019

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Seconds, Span}
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.ExecutionContext.Implicits.global

class Day07Spec extends AnyWordSpec with Matchers with ScalaFutures {

  "Run Amplifiers" should {
    "give the correct output for the given settings" in {
      val programmeCode = Vector(3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0)
      val amplifierSettings = Seq(4,3,2,1,0)

      Amplifiers.runAmplifiers(amplifierSettings, programmeCode) shouldBe 43210
    }

    "give the correct output for the test case 2" in {
      val programmeCode = Vector(3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0)
      val amplifierSettings = Seq(0,1,2,3,4)

      Amplifiers.runAmplifiers(amplifierSettings, programmeCode) shouldBe 54321
    }

    "give the correct output for the test case 3" in {
      val programmeCode = Vector(
        3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
        1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0)
      val amplifierSettings = Seq(1,0,4,3,2)

      Amplifiers.runAmplifiers(amplifierSettings, programmeCode) shouldBe 65210
    }
  }

  "Run Jerry Rigged Amplifiers" should {
    "give the correct output for test case 1" in {
      //implicit val defaultPatience: PatienceConfig =
      //PatienceConfig(timeout = Span(20, Seconds), interval = Span(5, Millis))

      val programmeCode = Vector(
        3, 26, 1001, 26, -4, 26, 3, 27, 1002, 27, 2, 27, 1, 27, 26,
        27, 4, 27, 1001, 28, -1, 28, 1005, 28, 6, 99, 0, 0, 5)
      val amplifierSettings = Seq(9, 8, 7, 6, 5)
      val futureOutput = Amplifiers.runJerryRiggedAmplifiers(amplifierSettings, programmeCode)

      whenReady(futureOutput, timeout(Span(600, Seconds))) {
        output => output shouldBe 139629729
      }
    }
  }

}
