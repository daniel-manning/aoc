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
    inputStack = new mutable.Stack().push(1))
  val endProgramme = diagnosticProgramme.runProgramme()

  println("Finished running diagnostics: Output ....")
  endProgramme.outputStack.foreach(println)

}
