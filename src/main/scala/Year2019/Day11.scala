package Year2019

import models.Position

import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
object Day11 extends App {

  val sourceCode = Source.fromResource("2019/day11")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(BigInt(_))

   val startRobot = HullPaintingRobot(Map.empty, Position(0,0), North)

  val list = HullPaintingRobot.startPainting(sourceCode, startRobot)

  val uniquePositions = list.toSet.toSeq

  println(s"Hull painting robot has painted ${uniquePositions.length} cells")


}
