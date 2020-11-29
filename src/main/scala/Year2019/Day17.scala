package Year2019

import Year2019.ProgrammeOperations.vectorProgrammeToMap
import models.Position

import scala.collection.mutable
import scala.io.Source

object Day17Test extends App {

  val map =
    """..#..........
      |..#..........
      |#######...###
      |#.#...#...#.#
      |#############
      |..#...#...#..
      |..#####...^..""".stripMargin

  val mapString = map.split('\n').toSeq.map(_.toSeq)
  val scaffold = Scaffolding.fromMap(mapString)
  val pointsOfIntersection = AlignmentParameters.findIntersections(scaffold)
  val alignmentParameters = AlignmentParameters.calculateAlignmentParameters(pointsOfIntersection)

  println(alignmentParameters)
}

object Day17 extends App {
  val sourceCode = Source.fromResource("2019/day17")
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

  val outputAscii = outputs.map(_.intValue.toChar)

  println(s"---------------------------")
  println(outputAscii)
  println(s"---------------------------")

  val mapString = outputAscii.mkString.split('\n').toSeq.map(_.toSeq)
  val scaffold = Scaffolding.fromMap(mapString)
  val pointsOfIntersection = AlignmentParameters.findIntersections(scaffold)
  val alignmentParameters = AlignmentParameters.calculateAlignmentParameters(pointsOfIntersection)
  println(alignmentParameters)
}


case class Scaffolding(map: Set[Position])
object Scaffolding {
  def fromMap(map: Seq[Seq[Char]]): Scaffolding =
    Scaffolding(map.zipWithIndex.flatMap {
      y => y._1.zipWithIndex.collect {
        x => x._1 match {
          case '#' => Position(x._2, y._2)
        }
      }
    }.toSet)

  def findSweeper(map: Seq[Seq[Char]]): Sweeper = {
    val position = map.zipWithIndex.flatMap {
      y => y._1.zipWithIndex.collect {
        x => x._1 match {
          case '^' => Position(x._2, y._2)
        }
      }
    }.head

    Sweeper(South, position)
  }
}

object AlignmentParameters {

  val neighbours = Seq(TwoVector(0, 1), TwoVector(1, 0), TwoVector(-1, 0), TwoVector(0, -1))

  def findIntersections(scaffoldMap: Scaffolding): Set[Position] = {
    //find points with exactly 4 neighbours
    scaffoldMap.map.filter {
      a =>
        val scaffoldNeighbours = (neighbours.map(x => a + x).toSet intersect scaffoldMap.map)
        scaffoldNeighbours.size == 4
    }
  }

  def calculateAlignmentParameters(intersections: Set[Position]): Int = intersections.map(p => p.x * p.y).sum
}