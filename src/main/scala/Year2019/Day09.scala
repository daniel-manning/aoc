package Year2019

import scala.io.Source
import Year2019.ProgrammeOperations.vectorProgrammeToMap

import scala.collection.mutable

object Day09 extends App {

  val sourceCode = Source.fromResource("2019/day09")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(n => BigInt(n.toInt))

  val inputQueue = new mutable.Queue[BigInt]()
  val outputQueue = new mutable.Queue[BigInt]()

  val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
    inputQueue = inputQueue.enqueue(1),
    outputQueue = outputQueue)

  val endProgramme = startProgramme.runProgramme()(RunningSettings("trial", debugOutput = true))

  val outputs = endProgramme.outputQueue.dequeueAll(_ => true)

  outputs.foreach(s => println(s"Output from programme: $s"))

  println("-----------part2----------------")

  val startCeresProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
    inputQueue = inputQueue.enqueue(2),
    outputQueue = outputQueue)

  val endCeresProgramme = startCeresProgramme.runProgramme()(RunningSettings("trial", debugOutput = true))

  val outputsForCeres = endCeresProgramme.outputQueue.dequeueAll(_ => true)

  outputsForCeres.foreach(s => println(s"Output from programme: $s"))

}


