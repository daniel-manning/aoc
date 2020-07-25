package Year2019

import models.Position

import scala.io.Source

object Day08 extends App {

  val spaceImageTransmission: Seq[Int] = Source.fromResource("2019/day08")
    .getLines()
    .toList
    .head
    .map(_.toString.toInt)

  val spaceImage = SpaceImageFormat.constructLayeredImage(spaceImageTransmission, (25, 6))

  val layer = spaceImage.layerWithTheFewestZeros
  val noOfOnes = layer.pixels.map(_.count(_ == 1)).sum
  val noOfTwos = layer.pixels.map(_.count(_ == 2)).sum

  println(s"The number of ones times the number of twos on the layer with the fewest zeros is: ${noOfOnes * noOfTwos}")

  val image = spaceImage.renderImage

  image.printOut()
}

sealed trait Colour
case object Black extends Colour
case object White extends Colour
case object Transparent extends Colour

object Colour {
  def apply(value: Int): Colour = value match {
    case 0 => Black
    case 1 => White
    case 2 => Transparent
    case _ => throw new Exception("Incorrect Colour Value!")
  }
}

case class RenderedImage(pixels:Seq[Seq[Colour]]){
  def printColour(c: Colour):String = c match {
    case Black => " "
    case White => "*"
    case Transparent => "T"
  }

  def printOut(): Unit =
    pixels.foreach { l =>
      println(s"${l.map(printColour).mkString}")
    }
}
case class SpaceImageLayer(pixels:Seq[Seq[Int]]) {
  def flattenWithPosition: Seq[(Colour, Position)] =
    for {
      (row, y) <- pixels.zipWithIndex
      (value, x) <- row.zipWithIndex
    } yield (Colour(value), Position(x, y))
}

case class SpaceImageFormat(layers:Seq[SpaceImageLayer]){

  def layerWithTheFewestZeros: SpaceImageLayer =
    layers.map(layer => (layer, layer.pixels.map(_.count(_ == 0)).sum)).minBy(_._2)._1

  def renderImage:RenderedImage = {
    val allPixels: Seq[(Int, Position, Colour)] = layers.zipWithIndex.flatMap(p => p._1.flattenWithPosition.map(x => (p._2, x._2, x._1)))
    val result: Seq[(Position, Seq[(Int, Colour)])] = allPixels.groupBy(_._2).map(p => (p._1, p._2.map(x => (x._1, x._3)).sortBy(_._1))).toSeq
    val colours: Seq[(Position, Colour)] = result.map(p => (p._1, findCorrectColour(p._2)))

   RenderedImage(ArraySupport.reconstructArray(colours))
  }

  def findCorrectColour(pixelLayers: Seq[(Int, Colour)]): Colour =
    pixelLayers.head match {
      case colour@((_, Black) | (_, White)) =>  colour._2
      case (_, Transparent) => findCorrectColour(pixelLayers.tail)
    }
}

object SpaceImageFormat {
  def constructLayeredImage(sequence: Seq[Int], dimension:(Int, Int)): SpaceImageFormat = {
    val layers = sequence.grouped(dimension._1).grouped(dimension._2).toSeq.map(layer => SpaceImageLayer(layer))

    SpaceImageFormat(layers)
  }
}
