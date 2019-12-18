package Year2019

import java.awt.Color

import Year2019.ProgrammeOperations.vectorProgrammeToMap

import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.{Dimension, Graphics2D, MainFrame, Panel, SimpleSwingApplication}

object Day13 extends SimpleSwingApplication {

  val sourceCode = Source.fromResource("2019/day13")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(BigInt(_))

    implicit val (ex1, queues) = ArcadeCabinet.loadGame(sourceCode)

  val tiles = ArcadeCabinet.grabFrame

  println(s"There are ${tiles.count(t => t.isInstanceOf[Block])} block tiles")

  lazy val ui: Panel = new Panel {
    background = Color.black
    preferredSize = new Dimension(700, 500)

    focusable = true


    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.setColor(Color.black)

      val offsetX = 100
      val offsetY = 250

      val width:Int = 10
      val height:Int = 10

      val drawWidth:Int = 9
      val drawHeight:Int = 9

      tiles.foreach {
        case Wall(x, y) =>
          g.setColor(Color.white)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case Block(x, y) =>
          g.setColor(Color.blue)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case HorizontalPaddle(x, y) =>
          g.setColor(Color.green)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case Ball(x, y) =>
          g.setColor(Color.red)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
        case EmptyTile(_, _) =>
      }

    }
  }

  def top: MainFrame = new MainFrame {
    title = "Arcade Game"
    contents = ui
  }

}


sealed trait Tile
case class EmptyTile(x: Int, y: Int) extends Tile
case class Wall(x: Int, y: Int) extends Tile
case class Block(x: Int, y: Int) extends Tile
case class HorizontalPaddle(x: Int, y: Int) extends Tile
case class Ball(x: Int, y: Int) extends Tile

object Tile {
  def apply(x:Int, y:Int, tileIdentifier: Int): Tile =
    tileIdentifier match {
      case 0 => EmptyTile(x, y)
      case 1 => Wall(x, y)
      case 2 => Block(x, y)
      case 3 => HorizontalPaddle(x, y)
      case 4 => Ball(x, y)
    }
}

object ArcadeCabinet {
  def grabFrame(implicit ex1: Future[IntCodeProgramme], queues: Queues): Seq[Tile] = {

    //block thread until programme exits or waits for Input
    while (!ex1.isCompleted || queues.waitingForInputQueue.nonEmpty) {
      Thread.sleep(20)
    }


    //remove any messages
    queues.waitingForInputQueue.dequeueAll(_ => true)

    //gather outputs
    val outputs = queues.outputQueue.dequeueAll(_ => true)

    outputs.sliding(3, 3).map(l => Tile(l(0).toInt, l(1).toInt, l(2).toInt)).toSeq
  }

  def loadGame(sourceCode: Vector[BigInt]): (Future[IntCodeProgramme], Queues) = {
    val queues = Queues(new mutable.Queue[BigInt](), new mutable.Queue[BigInt](), new mutable.Queue[BigInt]())

    val computerOne = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
      inputQueue = queues.inputQueue,
      outputQueue = queues.outputQueue)

    val ex1 = Future { computerOne.runProgramme()(RunningSettings("ArcadeGame", debugOutput = false))}

    (ex1, queues)
  }


}

