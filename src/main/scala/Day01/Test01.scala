package Day01

import scala.io.Source

object Test01 extends App {

  val frequencyChanges = Source.fromResource("input_day_01_01").getLines().toList.map(_.toInt)
  val outputFrequency = frequencyChanges.foldRight(0)(_+_)
  println(s"The Final Frequency is $outputFrequency")

}
