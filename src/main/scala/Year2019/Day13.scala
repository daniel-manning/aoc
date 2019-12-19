package Year2019

import java.awt.Color

import Year2019.ProgrammeOperations.vectorProgrammeToMap
import monix.execution.Scheduler.Implicits.global
import monix.reactive.Observable

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration.{Duration, MILLISECONDS}
import scala.io.Source
import scala.swing.{Dimension, Graphics2D, MainFrame, Panel, SimpleSwingApplication}

object Day13 extends SimpleSwingApplication {

  val sourceCode: Vector[BigInt] = Source.fromResource("2019/day13")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(BigInt(_))

  implicit val (ex1, queues) = ArcadeCabinet.loadGame(sourceCode)

  var tiles: Seq[Tile] = ArcadeCabinet.grabFrame

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

      val width: Int = 10
      val height: Int = 10

      val drawWidth: Int = 9
      val drawHeight: Int = 9

      tiles.foreach {
        case ScoreBoard(x, y, score) =>
          g.setColor(Color.orange)
          g.fillRect(offsetX + x * width, offsetY - y * height, drawWidth, drawHeight)
          g.setColor(Color.lightGray)
          g.drawString(score.toString, x, y)
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

  def repaint(): Unit = {
    queues.inputQueue.enqueue(NeutralPosition.value)
    queues.waitingForInputQueue.dequeueAll(_ => true)
    val updateTiles = ArcadeCabinet.updateFrame(tiles)
    //println(s"updateTiles: $updateTiles")
    if(updateTiles.nonEmpty){
      tiles = updateTiles
      ui.repaint()
    }
  }

  ///////////////MONIX TASKS
  val tick = {
    Observable.interval(Duration(20, MILLISECONDS))
      .map(x => repaint())
  }

  val cancelable = tick.subscribe()


}


sealed trait Tile{
  val x: Int
  val y: Int
}

case class EmptyTile(x: Int, y: Int) extends Tile

case class Wall(x: Int, y: Int) extends Tile

case class Block(x: Int, y: Int) extends Tile

case class HorizontalPaddle(x: Int, y: Int) extends Tile

case class Ball(x: Int, y: Int) extends Tile

case class ScoreBoard(x: Int, y: Int, score: BigInt) extends Tile

object Tile {
  def apply(x: Int, y: Int, tileIdentifier: Int): Tile =
    tileIdentifier match {
      case 0 => EmptyTile(x, y)
      case 1 => Wall(x, y)
      case 2 => Block(x, y)
      case 3 => HorizontalPaddle(x, y)
      case 4 => Ball(x, y)
    }
}

sealed trait JoyStickMovement {
  def value: Int
}

case object NeutralPosition extends JoyStickMovement {
  val value = 0
}

case object LeftTiltedPosition extends JoyStickMovement {
  val value: Int = -1
}

case object RightTiltedPosition extends JoyStickMovement {
  val value = 1
}

object ArcadeCabinet {

  private def getOutputs(implicit ex1: Future[IntCodeProgramme], queues: Queues): Seq[BigInt] = {
    //block thread until programme exits or waits for Input
    while (!ex1.isCompleted && queues.waitingForInputQueue.isEmpty) {
      println(queues.outputQueue.length)
      println(queues.waitingForInputQueue.length)
      Thread.sleep(20)
    }

    //remove any messages
    queues.waitingForInputQueue.dequeueAll(_ => true)

    //gather outputs
    queues.outputQueue.dequeueAll(_ => true)
  }

  def grabFrame(implicit ex1: Future[IntCodeProgramme], queues: Queues): Seq[Tile] = {
    val outputs = getOutputs
    println(s"outputs: $outputs")

    val minusOne = BigInt(-1)
    val zero = BigInt(0)

    outputs.sliding(3, 3).map {
      case Seq(`minusOne`, `zero`, score) => ScoreBoard(200, 300, score)
      case Seq(x, y, tileIdentifier) => Tile(x.toInt, y.toInt, tileIdentifier.toInt)
    }.toSeq
  }

  def updateFrame(tiles: Seq[Tile])(implicit ex1: Future[IntCodeProgramme], queues: Queues): Seq[Tile] = {

    val outputs = getOutputs

    println(s"outputs: $outputs")

    val minusOne = BigInt(-1)
    val zero = BigInt(0)

    val tileUpdates: Seq[Tile] = outputs.sliding(3, 3).map {
      case Seq(`minusOne`, `zero`, score) => ScoreBoard(200, 300, score)
      case Seq(x, y, tileIdentifier) => Tile(x.toInt, y.toInt, tileIdentifier.toInt)
    }.toSeq

    val tileSet = tiles.toSet
    val blocksBroken = tileUpdates.flatMap(t => tileSet.find(p => p.x == t.x && p.y == t.y))
    val ball = tileSet.filter(_.isInstanceOf[Ball])

    val result = tileSet
      .removedAll(blocksBroken)
      .removedAll(ball)
      .union(tileUpdates.toSet)
      .toSeq

    //println(s"result: $result")
    result
  }

  def loadGame(sourceCode: Vector[BigInt]): (Future[IntCodeProgramme], Queues) = {
    val queues = Queues(new mutable.Queue[BigInt](), new mutable.Queue[BigInt](), new mutable.Queue[BigInt]())

    val computerOne = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
      inputQueue = queues.inputQueue,
      outputQueue = queues.outputQueue,
      waitingForInputQueue = queues.waitingForInputQueue)

    val ex1 = Future {
      computerOne.runProgramme()(RunningSettings("ArcadeGame", debugOutput = false))
    }

    (ex1, queues)
  }


}

