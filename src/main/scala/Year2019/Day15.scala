package Year2019

import java.awt.Color
import java.awt.event.{KeyEvent, KeyListener}

import Year2019.ProgrammeOperations.vectorProgrammeToMap
import monix.reactive.Observable
import monix.execution.Scheduler.Implicits.global

import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source
import scala.concurrent.duration.{Duration, MILLISECONDS}
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

  var tiles: Seq[CraftTile] = Seq()
  implicit var robot: RepairDroid = RepairDroid(Position(0, 0))

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

//      println(tiles)

      //draw tiles
      tiles.foreach {
        case EmptySpace(Position(x, y)) =>
          g.setColor(Color.red)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case CraftWall(Position(x, y)) =>
          g.setColor(Color.white)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case OxygenUnit(Position(x, y)) =>
          g.setColor(Color.yellow)
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

    ui.peer.addKeyListener(new KeyListener(){
      def keyPressed(e:KeyEvent){
        //println("HELLO!")
        val direction = e.getKeyCode match {
          case 37 => West
          case 38 => North
          case 39 => East
          case 40 => South
        }

        val (newRobot, newTile) =  RepairDroid.setDirection(direction)

        //update things
        robot = newRobot
        tiles = tiles :+ newTile
      }

      def keyReleased(e:KeyEvent): Unit = {}
      def keyTyped(e:KeyEvent): Unit = {}
    })
  }

  def repaint(): Unit = {
      ui.repaint()
  }

  ///////////////MONIX TASKS
  val tick = {
    Observable.interval(Duration(400, MILLISECONDS))
      .map(x => repaint())
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
case class CraftWall(position: Position) extends CraftTile
case class EmptySpace(position: Position) extends CraftTile
case class OxygenUnit(position: Position) extends CraftTile



case class RepairDroid(position: Position)

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

   def setDirection(direction: Orientation)(implicit ex1: Future[IntCodeProgramme], queues: Queues, robot: RepairDroid): (RepairDroid, CraftTile) = {
     direction match {
       case North => queues.inputQueue.enqueue(BigInt(1))
       case South => queues.inputQueue.enqueue(BigInt(2))
       case West => queues.inputQueue.enqueue(BigInt(3))
       case East => queues.inputQueue.enqueue(BigInt(4))
     }

     grabKeyAndWait(direction)
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

  def grabKeyAndWait(direction: Orientation)(implicit ex1: Future[IntCodeProgramme], queues: Queues, robot: RepairDroid): (RepairDroid, CraftTile) = {
    val outputs = getOutputs

    //println(s"got ${outputs.length} outputs")

    val i = outputs.head
    DroidStatus(i) match {

      case BlockedByWall => {
        //println(s"Blocked by wall ${robot.position}")
        (robot, CraftWall(walk(robot.position, direction)))
      }
      case Moved => {
        //println(s"Moved ${robot.position}")
        //move the robot
        (robot.copy(position = walk(robot.position, direction)), EmptySpace(walk(robot.position, direction)))
      }
      case MovedToOxygenUnit => {
        //println(s"Moved to oxygen unit ${robot.position}")
        //move the robot
        (robot.copy(position = walk(robot.position, direction)), OxygenUnit(walk(robot.position, direction)))
      }
    }
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