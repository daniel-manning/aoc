package Year2018

import scala.io.Source

object Day01 extends App {

  val frequencyChanges = Source.fromResource("2018/day01_01_input").getLines().toList.map(_.toInt)
  val outputFrequency = frequencyChanges.foldRight(0)(_+_)
  println(s"The Final Frequency is $outputFrequency")

}
