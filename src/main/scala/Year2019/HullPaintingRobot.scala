package Year2019

import Year2019.ProgrammeOperations.vectorProgrammeToMap

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

sealed trait Direction
case object LeftTurn extends Direction
case object RightTurn extends Direction

object Direction {
  def apply(turn: Int): Direction = turn match {
    case 0 => LeftTurn
    case 1 => RightTurn
  }
}

sealed trait Orientation{
  def vector: (Int, Int)
}
case object North extends Orientation{
  val vector = (0, 1)
}
case object East extends Orientation{
  val vector = (1, 0)
}
case object West extends Orientation{
  val vector = (-1, 0)
}
case object South extends Orientation{
  val vector = (0, -1)
}

object Orientation {

  def changeDirection(orientation: Orientation, direction: Direction): Orientation =
    (orientation, direction) match {
      case (North, RightTurn) => East
      case (East, RightTurn) => South
      case (South, RightTurn) => West
      case (West, RightTurn) => North
      case (North, LeftTurn) => West
      case (East, LeftTurn) => North
      case (South, LeftTurn) => East
      case (West, LeftTurn) => South
    }

}


case class Queues(inputQueue: mutable.Queue[BigInt],
                  outputQueue: mutable.Queue[BigInt],
                  waitingForInputQueue: mutable.Queue[BigInt])

case class HullPaintingRobot(hullTiles:Map[Position, Colour], position: Position, orientation: Orientation){


def readColour: Colour = hullTiles.getOrElse(position, Black)

  def paintTile(colour: Colour): Map[Position, Colour] =
    hullTiles.updated(position, colour)

  def run(queues: Queues): HullPaintingRobot = {
    val inputColour = colourToOutput(readColour)
    queues.inputQueue.enqueue(inputColour)
    while(queues.outputQueue.length < 1){
      Thread.sleep(10)
    }

    val newColour = Colour(queues.outputQueue.dequeue().toInt)
    val newTiles = paintTile(newColour)

    while(queues.outputQueue.length < 1){
      Thread.sleep(10)
    }

    val turn = Direction(queues.outputQueue.dequeue().toInt)
    val (newPosition, newOrientation) = turnThenMove(turn)

    HullPaintingRobot(newTiles, newPosition, newOrientation)
  }

  private def turnThenMove(turn:Direction): (Position, Orientation) = {
    val newOrientation = Orientation.changeDirection(orientation, turn)
    val newPosition = stepForward(newOrientation)

    (newPosition, newOrientation)

  }

  def stepForward(orientation: Orientation): Position =
    orientation match {
      case North => position.copy(y = position.y + 1)
      case East => position.copy(x = position.x + 1)
      case South => position.copy(y = position.y - 1)
      case West => position.copy(x = position.x - 1)
    }

  def colourToOutput(colour: Colour): Int = colour match {
    case Black => 0
    case White => 1
  }

}

object HullPaintingRobot {

  def startPainting(sourceCode: Vector[BigInt], startingRobot: HullPaintingRobot)(implicit ec: ExecutionContext): LazyList[Position] = {

    val queues = Queues(new mutable.Queue[BigInt](), new mutable.Queue[BigInt](), new mutable.Queue[BigInt]())

    val computerOne = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
      inputQueue = queues.inputQueue,
      outputQueue = queues.outputQueue)

    val ex1 = Future { computerOne.runProgramme()(RunningSettings("HullPainter", debugOutput = false))}

    LazyList.unfold(startingRobot){
      robot =>

        if(ex1.isCompleted) None
        else {
          println(s"Running robot position: ${robot.position}")
          val newRobot = robot.run(queues)
          //println(s"Robot tile map size: ${newRobot.hullTiles.size}")

          Some((robot.position, newRobot))
        }

    }
  }

  //functions for interactive mode with updated drawing
  def startComputer(sourceCode: Vector[BigInt])(implicit ec: ExecutionContext): (Queues, Future[IntCodeProgramme]) = {
    val queues = Queues(new mutable.Queue[BigInt](), new mutable.Queue[BigInt](), new mutable.Queue[BigInt]())

    val computerOne = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
      inputQueue = queues.inputQueue,
      outputQueue = queues.outputQueue)

    val ex1 = Future { computerOne.runProgramme()(RunningSettings("HullPainter", debugOutput = false))}

    (queues, ex1)
  }


  def interactivePainting(robot: HullPaintingRobot)(implicit queues: Queues, ex1: Future[IntCodeProgramme]): Option[HullPaintingRobot] = {
      if(ex1.isCompleted) None
      else {
        println(s"Running robot position: ${robot.position}")
        val newRobot = robot.run(queues)
        //println(s"Robot tile map size: ${newRobot.hullTiles.size}")
        Some(newRobot)
      }
    }
}
