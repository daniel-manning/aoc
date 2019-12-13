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

case class Rational(num: Int, denom: Int){
  def isInteger: Boolean = denom == 1

  def lowestTerms: Rational = {
    val multiplier = denom.sign.toInt
    val gcd = BigInt(num).gcd(denom).toInt

    Rational(multiplier * num / gcd, multiplier * denom / gcd)
  }

  def +(that: Rational): Rational =
    Rational(this.num*that.denom + that.num*this.denom, this.denom*that.denom).lowestTerms

  def *(that: Rational): Rational =
    Rational(this.num*that.num, this.denom*that.denom).lowestTerms

  def floor: Int = Math.floor(num.toDouble / denom).toInt

  def toBigDecimal: BigDecimal = if(denom != 0){ BigDecimal(num)/BigDecimal(denom) } else { BigDecimal(num.sign * Double.MaxValue) }
}

object Rational {
  implicit def fromInt(value: Int): Rational = Rational(value, 1)
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

  def isAsteroidVisible(base: Position, asteroid: Position): Boolean = {
    if (base == asteroid) {
      false
    } else if (base.x == asteroid.x) {
      val yRange = rangeFromOneObjectToOther(base, asteroid)(_.y)
      !yRange.exists(y => field.contains(Position(base.x, y)))
    } else if (base.y == asteroid.y) {
      val xRange: Range = rangeFromOneObjectToOther(base, asteroid)(_.x)
      !xRange.exists(x => field.contains(Position(x, base.y)))
    } else {
      val gradient = Rational(base.y - asteroid.y, base.x - asteroid.x)

      val xRange: Range = rangeFromOneObjectToOther(base, asteroid)(_.x)

      !xRange.exists { x =>
        val y = gradient * (x - base.x) + base.y

        if (y.isInteger) {
          field.contains(Position(x, y.num))
        } else false
      }
    }
  }

  def oneRevolutionOfLaserBlaster(base:Position): Seq[(Position, Int)] =
    field
      .keys.toSeq
      .filter(asteroid => isAsteroidVisible(base, asteroid))
      .map(asteroid => (asteroid, Rational(base.y - asteroid.y, base.x - asteroid.x).lowestTerms))
      .sortBy(l => Math.atan(l._2.toBigDecimal.toDouble))
      .zipWithIndex
      .map(l => (l._1._1, l._2 + 1))

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