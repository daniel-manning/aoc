package Year2019

import java.awt.Color

import Year2019.MazeSolverMine.{closeOffDeadEnds, searchFrom}
import Year2019.ProgrammeOperations.vectorProgrammeToMap
import models.Position
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.io.Source
import scala.swing.{Dimension, Graphics2D, MainFrame, Panel, SimpleSwingApplication}

object Day15 extends SimpleSwingApplication {

  val sourceCode: Vector[BigInt] = Source.fromResource("2019/day15")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(BigInt(_))

  implicit val (ex1, queues) = RepairDroid.loadProgramme(sourceCode)
  implicit var robot: RepairDroid = RepairDroid(Set(EmptySpace(Position(0, 0))), Position(0, 0), false)

  lazy val ui: Panel = new Panel {
    background = Color.black
    preferredSize = new Dimension(700, 500)

    focusable = true


    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.setColor(Color.black)

      val offsetX = 500
      val offsetY = 250

      val width: Int = 10
      val height: Int = 10

      val drawWidth: Int = 9
      val drawHeight: Int = 9

      //draw tiles
      robot.tiles.foreach {
        case EmptySpace(Position(x, y)) =>
          g.setColor(Color.red)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case CraftWall(Position(x, y)) =>
          g.setColor(Color.white)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case OxygenUnit(Position(x, y)) =>
          g.setColor(Color.yellow)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case DeadEnd(Position(x, y)) =>
          g.setColor(Color.blue)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case Unexplored(Position(x, y)) =>
          g.setColor(Color.pink)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)

      }

      //draw droid
      g.setColor(Color.green)
      g.fillRect(offsetX + robot.position.x * width, offsetY - robot.position.y * height, drawWidth, drawHeight)

    }
  }

  def top: MainFrame = new MainFrame {
    title = "RepairDroid"
    contents = ui
  }

  def repaint(): Unit = {
      ui.repaint()
  }

  ///////////////MONIX TASKS
  val tick = {
    Observable.interval(Duration(100, MILLISECONDS))
      .map(x => update())
  }

  def update() = {
      val result = RepairDroid.interactivePainting(robot)
      ui.repaint()
      if(result.isDefined){
        robot = result.get
      } else {
        robot = robot.copy(isFinished = true)
      }

    }


  val cancelable = tick.subscribe()

}

sealed trait DroidStatus
case object BlockedByWall extends DroidStatus
case object Moved extends DroidStatus
case object MovedToOxygenUnit extends DroidStatus

object DroidStatus {
  def apply(in: BigInt): DroidStatus = in.toInt match {
    case 0 => BlockedByWall
    case 1 => Moved
    case 2 => MovedToOxygenUnit
    case _ => throw new Exception("Invalid Repair Drone Status")
  }
}

sealed trait CraftTile {
  val position: Position
}
case class StartingCell(position: Position) extends CraftTile
case class CraftWall(position: Position) extends CraftTile
case class EmptySpace(position: Position) extends CraftTile
case class OxygenUnit(position: Position) extends CraftTile
case class Unexplored(position: Position) extends CraftTile
case class DeadEnd(position: Position) extends CraftTile

object CraftTile {
  implicit def ordering[A <: CraftTile]: Ordering[A] = new Ordering[A] {
    override def compare(x: A, y: A): Int = {
      (x, y) match {
        //prioritise going to unexplored places
        case (_, Unexplored(_)) => 1

        //I don't remember what this is trying to achieve
        case (Unexplored(_), _) => -1

        case (_, _) => 0
      }
    }
  }
}


case class RepairDroid(tiles: Set[CraftTile], position: Position, isFinished: Boolean){
  def run()(implicit ex1: Future[IntCodeProgramme], queues: Queues): Option[RepairDroid] = {
      println(s"looking for exit")

          //TODO: The outstanding problem is the droid needs to explore the maze fully
    
          val result = searchFrom(tiles, position)
          result match {
            case SearchSuccess => None
            case SearchFailure(directions) => {
              val newMaze = MazeSolverMine.closeOffDeadEnds(tiles, position, directions.toSet)

              //this is the hard bit - decide on direction to follow
              val sortedDirections: Seq[(Orientation, CraftTile)] = directions.sortBy{
                a => a._2
              }
              val direction: Orientation = sortedDirections.head._1

              //set direction and move
              val (newDroid, tile) = setDirection(direction)
              println(s"returned tile is: $tile")
              //
              val mazeWithTile = newMaze + tile

              println(s"mazeWithTile: ${mazeWithTile}")

              println(s"Moving droid to: $newDroid")

              Some(RepairDroid(mazeWithTile, newDroid.position, false))
            }
          }
      }

  def setDirection(direction: Orientation)(implicit ex1: Future[IntCodeProgramme], queues: Queues): (RepairDroid, CraftTile) = {
    println(s"Set direction: $direction")
    println(s"InputQueue length: ${queues.inputQueue.length}")

    direction match {
      case North => queues.inputQueue.enqueue(BigInt(1))
      case South => queues.inputQueue.enqueue(BigInt(2))
      case West => queues.inputQueue.enqueue(BigInt(3))
      case East => queues.inputQueue.enqueue(BigInt(4))
    }

    grabKeyAndWait(direction)
  }

  def grabKeyAndWait(direction: Orientation)(implicit ex1: Future[IntCodeProgramme], queues: Queues): (RepairDroid, CraftTile) = {
    val outputs = getOutputs

    println(s"got ${outputs.length} outputs")

    val i = outputs.head
    DroidStatus(i) match {

      case BlockedByWall => {
        //println(s"Blocked by wall ${robot.position}")
        (this, CraftWall(walk(position, direction)))
      }
      case Moved => {
        //println(s"Moved ${robot.position}")
        //move the robot
        (this.copy(position = walk(position, direction)), EmptySpace(walk(position, direction)))
      }
      case MovedToOxygenUnit => {
        //println(s"Moved to oxygen unit ${robot.position}")
        //move the robot
        (this.copy(position = walk(position, direction)), OxygenUnit(walk(position, direction)))
      }
    }
  }

  private def getOutputs(implicit ex1: Future[IntCodeProgramme], queues: Queues): Seq[BigInt] = {
    //block thread until programme exits or waits for Input
    while (!ex1.isCompleted && (queues.waitingForInputQueue.isEmpty || queues.outputQueue.isEmpty)) {
      Thread.sleep(20)
    }

    //remove any messages
    queues.waitingForInputQueue.dequeueAll(_ => true)

    //gather outputs
    queues.outputQueue.dequeueAll(_ => true)
  }

  private def walk(position: Position, direction: Orientation): Position = {
    direction match {
      case North => Position(position.x, position.y + 1)
      case South => Position(position.x, position.y - 1)
      case East => Position(position.x + 1, position.y)
      case West => Position(position.x - 1, position.y)
    }
  }
}

object RepairDroid {

  def loadProgramme(sourceCode: Vector[BigInt]): (Future[IntCodeProgramme], Queues) = {
    val queues = Queues(new mutable.Queue[BigInt](), new mutable.Queue[BigInt](), new mutable.Queue[BigInt]())

    val computerOne = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
      inputQueue = queues.inputQueue,
      outputQueue = queues.outputQueue,
      waitingForInputQueue = queues.waitingForInputQueue)

    val ex1 = Future {
      computerOne.runProgramme()(RunningSettings("RepairDroid", debugOutput = false))
    }

    (ex1, queues)
  }

  def interactivePainting(robot: RepairDroid)(implicit queues: Queues, ex1: Future[IntCodeProgramme]): Option[RepairDroid] = {
    if(ex1.isCompleted) None
    else if(robot.isFinished){
      //count up the active squares and output
      val count = robot.tiles.flatMap {
        case a: EmptySpace => Some(a)
        case _   => None
      }.size

      println(s"The number of squares on the route is: $count")
      None
    }else {
      println(s"Running robot position: ${robot.position}")
      val newRobot = robot.run()
      //println(s"Robot tile map size: ${newRobot.hullTiles.size}")
      newRobot
    }
  }
}