package Day01

import scala.io.Source

object Day01_02Loader extends App {
  val frequencyChanges = Source.fromResource("day_01_01_input").getLines().toList.map(_.toInt)
  val firstRepeat = Callibration.scanList(0, frequencyChanges)
  println(s"First repeated frequency is $firstRepeat")
}

object Callibration {

  def scanList(startingFrequency:Int, frequencyShifts:List[Int]):Int = {
    val firstRepeat = Stream.continually(frequencyShifts).flatten
      .scanLeft((List[Int](), startingFrequency))( (lF,fc) =>
            (lF._2 :: lF._1, lF._2 + fc)
      ).find(p => p._1.contains(p._2))

    firstRepeat.map(_._2).getOrElse(Int.MinValue)
  }

}