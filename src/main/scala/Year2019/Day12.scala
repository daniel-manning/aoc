package Year2019

import scala.io.Source

object Day12 extends App {
  val vectors = Source.fromResource("2019/day12")
    .getLines()
    .toList
    .map(s => ThreeVectorParser.parse(ThreeVectorParser.tvp, s).get)

  val startingNBodySystem = NBodySystem(vectors.zipWithIndex.map(v => SystemBody(v._2.toString, v._1)))
}

case class SystemBody(label: String, position: ThreeVector, velocity: ThreeVector = ThreeVector(0, 0, 0)){
  def moveBody: SystemBody =
    this.copy(position = ThreeVector(
      velocityOnAxis((vector: ThreeVector) => vector.x),
      velocityOnAxis((vector: ThreeVector) => vector.y),
      velocityOnAxis((vector: ThreeVector) => vector.z)))

  def velocityOnAxis(axis: ThreeVector => Int): Int =
    axis(position) + axis(velocity)

  def potentialEnergy: Int =
    Math.abs(position.x) + Math.abs(position.y) + Math.abs(position.z)

  def kineticEnergy: Int =
    Math.abs(velocity.x) + Math.abs(velocity.y) + Math.abs(velocity.z)
}

object SystemBody {
  def considerPairs(bodyOne: SystemBody, bodyTwo: SystemBody): ((String, ThreeVector), (String, ThreeVector)) = {
    ((bodyOne.label, ThreeVector(
      gravityOnAxis(bodyOne.position.x, bodyTwo.position.x),
      gravityOnAxis(bodyOne.position.y, bodyTwo.position.y),
      gravityOnAxis(bodyOne.position.z, bodyTwo.position.z)
    )),
      (bodyTwo.label, ThreeVector(
        gravityOnAxis(bodyTwo.position.x, bodyOne.position.x),
        gravityOnAxis(bodyTwo.position.y, bodyOne.position.y),
        gravityOnAxis(bodyTwo.position.z, bodyOne.position.z)
      ))
    )
  }

  def gravityOnAxis(mainDistance: Int, oppositeDistance: Int): Int =
    -mainDistance.compare(oppositeDistance)
}

case class NBodySystem(system: Seq[SystemBody], timeStep: Int = 0) {
  def applyGravity: NBodySystem = {
    val pairs = SeqOps.crossDifferentPairs(system)
    println(pairs)
    val velocityUpdates: Map[String, ThreeVector] = pairs
      .map(p => SystemBody.considerPairs(p._1, p._2))
      .flatMap(p => Seq(p._1, p._2))
        .groupBy(_._1).map{
      l =>
        val effectiveVelocity: ThreeVector = l._2.foldLeft(ThreeVector(0,0,0)){
          (a,b) => a + b._2
        }

        (l._1, effectiveVelocity)
    }

     val updatedBodies = system.map {
       body => body
         .copy(velocity = body.velocity + velocityUpdates(body.label))
         .moveBody
     }.sortBy(_.label)

    NBodySystem(updatedBodies, timeStep + 1)
  }
}

object NBodySystem {
  def evolveUntilTime(body: NBodySystem, untilTime: Int): NBodySystem = {
    LazyList.unfold(body) {
      system =>
        if (system.timeStep == untilTime) None
        else {
          val newSystem = system.applyGravity
          Some((newSystem, newSystem))
        }
    }.last
  }
}


