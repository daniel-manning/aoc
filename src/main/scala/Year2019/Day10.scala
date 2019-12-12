package Year2019

object Day10 extends App {

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

case class AsteroidField(field: Map[Position, Space])

object AsteroidField {
  def construct(field: Seq[String]): AsteroidField = {
   val asteroidField = ArraySupport.sequenceArrayToPositionSeq(field.map(_.toSeq))(Space.fromChar).toMap
    //println(asteroidField)
    AsteroidField(asteroidField)
  }
}