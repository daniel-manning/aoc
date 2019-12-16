package Year2019

import scala.util.parsing.combinator.RegexParsers

case class TwoVector(x:Int, y: Int){

  def dotProduct(that: TwoVector): Int =
    this.x*that.x + this.y*that.y

  def norm: Double = Math.sqrt(x*x + y*y)
}

object TwoVector {

  //this only works against an axis vector
  def calculateAngle(vecOne:TwoVector, vecTwo: TwoVector): Double = {
    val result = Math.acos(vecOne.dotProduct(vecTwo) / (vecOne.norm * vecTwo.norm))

    if(vecOne.x < 0) 2*Math.PI - result
    else result
  }
}

case class ThreeVector(x:Int, y: Int, z: Int)

object ThreeVectorParser extends RegexParsers {
  def bracket: Parser[String]    = """[<>]+""".r ^^ { _.toString }
  def axisLabel: Parser[String]    = """[a-z]=""".r ^^ { _.toString }
  def number: Parser[Int] = """[-0-9]+""".r ^^ { _.toInt }
  def comma: Parser[String]    = """,""".r ^^ { _.toString }

  def tvp: Parser[ThreeVector] =
    bracket ~
      axisLabel ~ number ~ comma ~
      axisLabel ~ number ~ comma ~
      axisLabel ~ number ~ bracket ^^ { case _ ~ _ ~ x ~ _ ~ _ ~ y ~ _ ~ _ ~ z ~ _ => ThreeVector(x, y, z) }
}
