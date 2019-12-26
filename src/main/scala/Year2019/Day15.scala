package Year2019

import java.awt.Color

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

   val tiles: Seq[CraftTile] = Seq()

  lazy val ui: Panel = new Panel {
    background = Color.black
    preferredSize = new Dimension(700, 500)

    focusable = true


    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.setColor(Color.black)

      val offsetX = 100
      val offsetY = 250

      val width: Int = 10
      val height: Int = 10

      val drawWidth: Int = 9
      val drawHeight: Int = 9

      tiles.foreach {
        case EmptySpace(Position(x, y)) =>
          g.setColor(Color.lightGray)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case CraftWall(Position(x, y)) =>
          g.setColor(Color.white)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case OxygenUnit(Position(x, y)) =>
          g.setColor(Color.yellow)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
      }

    }
  }

  def top: MainFrame = new MainFrame {
    title = "RepairDroid"
    contents = ui
  }

  def repaint(): Unit = {
        ???
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

sealed trait CraftTile {
  val position: Position
}
case class CraftWall(position: Position) extends CraftTile
case class EmptySpace(position: Position) extends CraftTile
case class OxygenUnit(position: Position) extends CraftTile


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

   def setDirection(direction: Orientation, queues: Queues): Unit = {
     direction match {
       case North => queues.inputQueue.enqueue(BigInt(1))
       case South => queues.inputQueue.enqueue(BigInt(2))
       case West => queues.inputQueue.enqueue(BigInt(3))
       case East => queues.inputQueue.enqueue(BigInt(4))
     }
   }

  private def getOutputs(implicit ex1: Future[IntCodeProgramme], queues: Queues): Seq[BigInt] = {
    //block thread until programme exits or waits for Input
    while (!ex1.isCompleted && queues.waitingForInputQueue.isEmpty) {
      Thread.sleep(20)
    }

    //remove any messages
    queues.waitingForInputQueue.dequeueAll(_ => true)

    //gather outputs
    queues.outputQueue.dequeueAll(_ => true)
  }


}