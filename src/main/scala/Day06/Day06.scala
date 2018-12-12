package Day06

//type Point = (Int, Int)

object Day06 extends App{

}



object Distance {
  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]) = for { x <- xs; y <- ys } yield (x, y)
  }

  def manhattanDistance(pointOne:(Int, Int), pointTwo:(Int, Int)):Int = {
    Math.abs(pointOne._1 - pointTwo._1) + Math.abs(pointOne._2 - pointTwo._2)
  }

  def generateGrid(width:Int, height:Int):List[(Int, Int)] = {
    val rangeWidth = (0 until width)
    val rangeHeight = (0 until height)

    rangeHeight cross rangeWidth toList
  }

}
