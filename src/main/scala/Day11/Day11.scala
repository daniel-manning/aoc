package Day11

object Day11 {

}

object FuelCell {

  val arrows = List((0, 0), (0, 1), (0, 2),
                    (1, 0), (1, 1), (1, 2),
                    (2, 0), (2, 1), (2, 2))

  def findlargestTotalPowerSquare(gridSerialNumber: Int):(Location, Option[Int]) = {
    val powers:List[(Location, Int)] = ((1 to 300) cross (1 to 300)).toList.map(p => (p, determineCellPower(p, gridSerialNumber)))

    val totals:List[(Location, Option[Int])] = powers.map(p => (p._1, total(p._1, powers)))

    totals.filter(_._2.isDefined).maxBy(_._2.get)
  }


  def determineCellPower(location:Location, gridSerialNumber:Int): Int ={

    val rackID = location._1 + 10
    val powerLevel = rackID * location._2
    val powerLevelStep2 = powerLevel + gridSerialNumber
    val powerLevelStep3 = powerLevelStep2*rackID

    powerLevelStep3.toString.reverse.drop(2).head.toString.toInt - 5
  }

  def total(start:Location, powers:List[(Location, Int)]):Option[Int] = {
    arrows.map(a => (start._1 + a._1, start._2 + a._2)).foldRight(Some(0):Option[Int]){
      (a, b) => powers.find(p => p._1 == a).flatMap(c => b match {
        case Some(d) => Some(d + c._2)
        case _ => None
      })
    }

  }


}