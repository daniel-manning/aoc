package Day11

import org.scalatest.{Matchers, WordSpec}

class Day11Spec extends WordSpec with Matchers{

  "PowerCellCalculator" should {
    "calculate power for a given cell" in {

      val location = (3,5)
      val gridSerialNumber = 8

      FuelCell.determineCellPower(location, gridSerialNumber) shouldBe 4
    }

    "calculate other cells" in {
      FuelCell.determineCellPower((122,79), 57) shouldBe -5
      FuelCell.determineCellPower((217,196), 39) shouldBe 0
      FuelCell.determineCellPower((101,153), 71) shouldBe 4
    }

    "find square of largest total power for grid 18" in  {
      FuelCell.findlargestTotalPowerSquare(gridSerialNumber = 18) shouldBe ((33,45), Some(29))
    }

    "total a square of powers" in {
      val powers = List(((1,1),4), ((2,1),4), ((3,1),4),
                        ((1,2),3), ((2,2),3), ((3,2),4),
                        ((1,3),1), ((2,3),2), ((3,3),4))

      val location = (1,1)

      FuelCell.total(location, powers) shouldBe Some(29)
    }
  }

}
