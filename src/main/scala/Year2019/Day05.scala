package Year2019

import Year2019.ProgrammeOperations.vectorProgrammeToMap

import scala.collection.mutable
import scala.io.Source

object Day05 extends App {

  val diagnosticProgrammeSource: Vector[BigInt] =
    Source.fromResource("2019/day05")
      .getLines()
      .toList
      .head
    .split(",")
    .toVector
    .map(n => BigInt(n.toInt))

  val diagnosticProgramme = IntCodeProgramme(
    programme = vectorProgrammeToMap(diagnosticProgrammeSource),
    inputQueue = new mutable.Queue().enqueue(BigInt(1)))
  val endProgramme = diagnosticProgramme.runProgramme()

  println("Part 1: Finished running diagnostics: Output ....")
  endProgramme.outputQueue.foreach(println)

  val diagnosticThermalProgramme = IntCodeProgramme(
    programme = vectorProgrammeToMap(diagnosticProgrammeSource),
    inputQueue = new mutable.Queue().enqueue(BigInt(5)))
  val endThermalProgramme = diagnosticThermalProgramme.runProgramme()
  println("Part 2: Finished running diagnostics: Output ....")
  endThermalProgramme.outputQueue.foreach(println)

}
