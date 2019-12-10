package Year2019

import scala.collection.mutable
import scala.io.Source

object Day05 extends App {

  val diagnosticProgrammeSource: Vector[Int] =
    Source.fromResource("2019/day05")
      .getLines()
      .toList
      .head
    .split(",")
    .toVector
    .map(_.toInt)

  val diagnosticProgramme = IntCodeProgramme(
    programme = diagnosticProgrammeSource,
    inputQueue = new mutable.Queue().enqueue(1))
  val endProgramme = diagnosticProgramme.runProgramme()

  println("Part 1: Finished running diagnostics: Output ....")
  endProgramme.outputQueue.foreach(println)

  val diagnosticThermalProgramme = IntCodeProgramme(
    programme = diagnosticProgrammeSource,
    inputQueue = new mutable.Queue().enqueue(5))
  val endThermalProgramme = diagnosticThermalProgramme.runProgramme()
  println("Part 2: Finished running diagnostics: Output ....")
  endThermalProgramme.outputQueue.foreach(println)

}
