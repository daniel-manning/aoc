package Day01

import org.scalatest.FeatureSpec

class Day01_02Spec extends FeatureSpec {

  feature("Callibrator") {
    scenario("finds the correct repeat of a list of two") {
      val startingFrequency = 0
      val frequencyShifts = List(1, -1)

      assert(Callibration.scanList(startingFrequency, frequencyShifts) == 0)
    }

    scenario("finds the correct repeat of a list of six") {
      val startingFrequency = 0
      val frequencyShifts = List(1, -2, 3, 1, 1, -2)

      assert(Callibration.scanList(startingFrequency, frequencyShifts) == 2)
    }

    scenario("finds the correct repeat of a list of five") {
      val startingFrequency = 0
      val frequencyShifts = List(3, 3, 4, -2, -4)

      assert(Callibration.scanList(startingFrequency, frequencyShifts) == 10)
    }

    scenario("finds the correct repeat of a list") {
      val startingFrequency = 0
      val frequencyShifts = List(-6, 3, 8, 5, -6)

      assert(Callibration.scanList(startingFrequency, frequencyShifts) == 5)
    }

    scenario("finds the correct repeat of another list") {
      val startingFrequency = 0
      val frequencyShifts = List(7, 7, -2, -7, -4)

      assert(Callibration.scanList(startingFrequency, frequencyShifts) == 14)
    }
  }


}
