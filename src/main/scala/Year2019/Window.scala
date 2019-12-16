package Year2019

import java.awt.Color

import monix.reactive.Observable

import scala.concurrent.duration.{Duration, _}
import scala.io.Source
import scala.swing.{Dimension, Graphics2D, MainFrame, Panel, SimpleSwingApplication}
import monix.execution.Scheduler.Implicits.global

object Window extends SimpleSwingApplication {
  val sourceCode = Source.fromResource("2019/day11")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(BigInt(_))

  var robot = HullPaintingRobot(Map(Position(0,0) -> White), Position(0,0), North)

  implicit val (queues, execution) = HullPaintingRobot.startComputer(sourceCode)

  lazy val ui: Panel = new Panel {
    background = Color.black
    preferredSize = new Dimension(700, 700)

    focusable = true


    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      g.setColor(Color.black)

      val offsetX = 400
      val offsetY = 250

      val width:Int = 10
      val height:Int = 10

      val drawWidth:Int = 9
      val drawHeight:Int = 9

      robot.hullTiles.foreach(x => {
        val (location, colour) = x

        colour match {
          case Black => g.setColor(Color.black)
          case White => g.setColor(Color.white)
        }

        g.fillRect(offsetX + location.x * width, offsetY - location.y * height, drawWidth, drawHeight)
      })

    }
  }

  def top: MainFrame = new MainFrame {
    title = "Hull Painting Robot"
    contents = ui
  }

  ///////////////MONIX TASKS
  val tick = {
    Observable.interval(Duration(20, MILLISECONDS))
      .map(x => {
        robot = {
          val result = HullPaintingRobot.interactivePainting(robot)
          ui.repaint()
          result.get
        }
      })
  }

  val cancelable = tick.subscribe()
}
