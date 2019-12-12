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
}

object Rational {
  implicit def fromInt(value: Int): Rational = Rational(value, 1)
}

case class AsteroidField(field: Map[Position, Space]){

  def noOfAsteroidsVisibleFromPosition(base: Position): Int = {
      field.count { a =>
        val asteroidsInLineBefore =
          if (base == a._1) {
            1
          } else if (base.x == a._1.x) {
            val yRange = if (base.y > a._1.y) {
              ((a._1.y + 1) until base.y)
            } else {
              ((base.y + 1) until a._1.y)
            }

            yRange.count(y => field.contains(Position(base.x, y)))
          } else if (base.y == a._1.y) {
            val xRange: Range = if (base.x > a._1.x) {
              ((a._1.x + 1) until base.x)
            } else {
              ((base.x + 1) until a._1.x)
            }

            xRange.count(x => field.contains(Position(x, base.y)))
          } else {
            val gradient = Rational(base.y - a._1.y, base.x - a._1.x)

            val xRange: Range = if (base.x > a._1.x) {
              ((a._1.x + 1) until base.x)
            } else {
              ((base.x + 1) until a._1.x)
            }

            xRange.count { x =>
              val y = gradient * (x - base.x) + base.y

              if (y.isInteger) {
                field.contains(Position(x, y.num))
              } else false
            }
          }

        asteroidsInLineBefore == 0
      }
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