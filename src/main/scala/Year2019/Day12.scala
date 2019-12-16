package Year2019

import scala.io.Source

object Day12 extends App {
  val vectors = Source.fromResource("2019/day12")
    .getLines()
    .toList
    .map(s => ThreeVectorParser.parse(ThreeVectorParser.tvp, s).get)

  val startingNBodySystem = NBodySystem(vectors.map(v => SystemBody(v)))
}

case class SystemBody(position: ThreeVector, velocity: ThreeVector = ThreeVector(0, 0, 0))

case class NBodySystem(system: Seq[SystemBody]){
  def applyGravity: NBodySystem = {
    SeqOps.crossDifferentPairs(system)
    ???
  }
}


