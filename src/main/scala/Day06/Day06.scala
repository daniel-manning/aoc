package Day06

import scala.io.Source
import scala.language.postfixOps

//type Point = (Int, Int)

object Day06 extends App{
  val targets:List[(Int, Int)] = Source.fromResource("day06_input").getLines.toList.map(s => s.split(",").toList match {
    case List(a, b) => (a.trim.toInt, b.trim.toInt)
  })

  /*val results = Distance.calculateAreas(targets).sortBy(_._2)
  println(s"Areas are: $results")*/
  val largestArea = Distance.calculateAreas(targets).maxBy(_._2)
  println(s"Largest finite are is ${largestArea._2} which is target ${targets(largestArea._1)}")
}

object Day06_02 extends App{
  val targets:List[(Int, Int)] = Source.fromResource("day06_input").getLines.toList.map(s => s.split(",").toList match {
    case List(a, b) => (a.trim.toInt, b.trim.toInt)
  })

  val regionOfMinimumDistance = Distance.calcuateRegion(targets, 10000)
  println(s"Area of the region of minimum distance is ${regionOfMinimumDistance.size}")
}



object Distance {
  implicit class Crossable[X](xs: Traversable[X]) {
    def cross[Y](ys: Traversable[Y]): Traversable[(X, Y)] = for {x <- xs; y <- ys } yield (x, y)
  }

  def manhattanDistance(pointOne:(Int, Int), pointTwo:(Int, Int)):Int = {
    Math.abs(pointOne._1 - pointTwo._1) + Math.abs(pointOne._2 - pointTwo._2)
  }

  def generateGrid(width:Int, height:Int):List[(Int, Int)] = {
    val rangeWidth = (0 until width)
    val rangeHeight = (0 until height)

    rangeHeight cross rangeWidth toList
  }

  def calculateAreas(targets:List[(Int, Int)]):List[(Int, Int)] = {
    val maxWidth = targets.maxBy(_._1)._1 + 1
    val maxHeight = targets.maxBy(_._2)._2 + 1

    val grid = generateGrid(maxWidth, maxHeight)



    grid.map(p => (p, targetClosestTo(p, targets)))
      .filter(p => p._2.isDefined)
      .groupBy(p => p._2)
      .filterNot(p => areaIsUnbounded(p._2, maxWidth, maxHeight))
      .toList.map(p => (p._1.get, p._2.size))
  }

  def areaIsUnbounded(tuples: List[((Int, Int), Option[Int])], width:Int, height:Int):Boolean = {
    tuples.exists(p => p._1._1 == 0 || p._1._2 == 0 || p._1._1 == width || p._1._1 == height)
  }

  def targetClosestTo(point:(Int,Int), targets:List[(Int, Int)]):Option[Int] = {
    val minVal:Int = targets.map(target => manhattanDistance(point, target)).min
    val contenders:List[(Int, Int)] = targets.zipWithIndex.map(p => (p._2, manhattanDistance(point, p._1))).filter(p => p._2 == minVal)

    if(contenders.size > 1) None else Some(contenders.head._1)
  }

  def distanceToAllTargets(point:(Int, Int), targets:List[(Int, Int)]):Int = {
    targets.map(p => manhattanDistance(p, point)).sum
  }

  def calcuateRegion(targets:List[(Int, Int)], maxDistance:Int):List[(Int,Int)] = {
    val maxWidth = targets.maxBy(_._1)._1 + 1
    val maxHeight = targets.maxBy(_._2)._2 + 1

    val grid = generateGrid(maxWidth, maxHeight)

    grid.filter(p => distanceToAllTargets(p, targets) < maxDistance)

  }

}
