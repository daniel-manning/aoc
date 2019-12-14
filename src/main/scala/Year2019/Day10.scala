package Year2019

import scala.io.Source

object Day10 extends App {
  val asteroidSeq: Seq[String] = Source.fromResource("2019/day10")
    .getLines()
    .toList


  val asteroidField = AsteroidField.construct(asteroidSeq)

  val (bestLocation, asteroidsCovered) = asteroidField.findBestPositionForAsteroidBase

  println(s"The best position in the asteroid field is $bestLocation where it observes $asteroidsCovered asteroids")

}

sealed trait Space
case object Empty extends Space
case object Asteroid extends Space

object Space {
  def fromChar(c: Char): Space =
    c match {
      case '.' => Empty
      case '#' => Asteroid
    }
}

case class AsteroidField(field: Map[Position, Space]){

  def noOfAsteroidsVisibleFromPosition(base: Position): Int = {
      field.count { a =>
        isAsteroidVisible(base, a._1)
      }
  }

  private def rangeFromOneObjectToOther(base: Position, asteroid: Position)(direction:Position => Int): Range = {
    if (direction(base) > direction(asteroid)) {
      (direction(asteroid) + 1) until direction(base)
    } else {
      (direction(base) + 1) until direction(asteroid)
    }
  }

  def isAsteroidVisible(base: Position, asteroid: Position, asteroids: Seq[Position] = field.keys.toSeq): Boolean = {
    if (base == asteroid) {
      false
    } else if (base.x == asteroid.x) {
      val yRange = rangeFromOneObjectToOther(base, asteroid)(_.y)
      !yRange.exists(y => asteroids.contains(Position(base.x, y)))
    } else if (base.y == asteroid.y) {
      val xRange: Range = rangeFromOneObjectToOther(base, asteroid)(_.x)
      !xRange.exists(x => asteroids.contains(Position(x, base.y)))
    } else {
      val gradient = Rational(base.y - asteroid.y, base.x - asteroid.x)

      val xRange: Range = rangeFromOneObjectToOther(base, asteroid)(_.x)

      !xRange.exists { x =>
        val y = gradient * (x - base.x) + base.y

        if (y.isInteger) {
          asteroids.contains(Position(x, y.num))
        } else false
      }
    }
  }

  def oneRevolutionOfLaserBlaster(base:Position, asteroids:Seq[Position] = field.keys.toSeq): Seq[(Position, Int)] =
    asteroids
      .filter(asteroid => isAsteroidVisible(base, asteroid, asteroids))
      .map(asteroid => (asteroid, TwoVector(asteroid.x - base.x, asteroid.y - base.y)))
      .sortBy(l => TwoVector.calculateAngle(l._2, TwoVector(0, -1)))
      .zipWithIndex
      .map(l => (l._1._1, l._2 + 1))

  def removeAsteroidsWithLaserBlaster(base:Position): Seq[(Position, Int)] = {
    Seq.unfold((field.keys.toSeq, 0)){
      a =>
        val blastedAsteroids = oneRevolutionOfLaserBlaster(base, a._1).map(l => (l._1, l._2 + a._2))

        val asteroidsLeft =  a._1.toSet.diff(blastedAsteroids.map(_._1).toSet).toSeq

        //println(s"blastedAsteroids: $blastedAsteroids")
        //println(s"asteroidsLeft: $asteroidsLeft")

        if(a._1.length > 1) Some((blastedAsteroids, (asteroidsLeft, blastedAsteroids.last._2)))
        else None
    }.flatten
  }


  def scorePositionsForAsteroidBase: Seq[(Position, Int)] =
    field.toSeq.sortBy(l => (l._1.x, l._1.y)).map(p => (p._1, noOfAsteroidsVisibleFromPosition(p._1)))


  def findBestPositionForAsteroidBase: (Position, Int) =
    scorePositionsForAsteroidBase.maxBy(_._2)
}

object AsteroidField {
  def construct(field: Seq[String]): AsteroidField = {
   val asteroidField = ArraySupport.sequenceArrayToPositionSeq(field.map(_.toSeq))(Space.fromChar).filter {
     sp => sp._2 match {
       case Asteroid => true
       case _ => false
     }
   }.toMap

    AsteroidField(asteroidField)
  }
}