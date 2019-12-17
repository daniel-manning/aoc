package Year2019

import Year2019.NBodySystem.areTwoBodiesTheSamePositionAndVelocityOnAxis
import utils.LazyPrimeFactors

import scala.io.Source

object Day12 extends App {
  val vectors = Source.fromResource("2019/day12")
    .getLines()
    .toList
    .map(s => ThreeVectorParser.parse(ThreeVectorParser.tvp, s).get)

  val startingNBodySystem = NBodySystem(vectors.zipWithIndex.map(v => SystemBody(v._2.toString, v._1)))

  val energyAfter1000Steps = NBodySystem.evolveUntilTime(startingNBodySystem, 1000)
    .totalEnergy

  println(s"Energy of the system after 1000 steps is $energyAfter1000Steps")

  val cycleTimes = NBodySystem.returnTimeCycles(startingNBodySystem)
  val returnTime = NBodySystem.returnTime(startingNBodySystem)

  println(s"The return time cycles of the system is $cycleTimes")
  println(s"The return time of the entire system is $returnTime")
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

  def totalEnergy: Int = potentialEnergy * kineticEnergy
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

  def totalEnergy: Int = system.map(_.totalEnergy).sum
}

object NBodySystem {
  def returnTimeCycles(startingNBodySystem: NBodySystem): (Option[Int], Option[Int], Option[Int]) = {
    //All 3 coordinates are independent - find the cycles for each
    //Full return time is the least common multiple
    LazyList.unfold((1, (Option.empty[Int], Option.empty[Int], Option.empty[Int]), startingNBodySystem)) {
      system =>
        val (cycleOne, cycleTwo, cycleThree) = system._2
        if (cycleOne.isDefined && cycleTwo.isDefined && cycleThree.isDefined) None
        else {
          val newSystem = system._3.applyGravity

          val newCycleOne = returnTimeOfCycleOnAxis(cycleOne, system._1, startingNBodySystem, newSystem, (vector: ThreeVector) => vector.x)

          val newCycleTwo = returnTimeOfCycleOnAxis(cycleTwo, system._1, startingNBodySystem, newSystem, (vector: ThreeVector) => vector.y)

          val newCycleThree = returnTimeOfCycleOnAxis(cycleThree, system._1, startingNBodySystem, newSystem, (vector: ThreeVector) => vector.z)

          Some(((newCycleOne, newCycleTwo, newCycleThree), (system._1 + 1, (newCycleOne, newCycleTwo, newCycleThree), newSystem)))
        }
    }.last
  }

  def returnTime(startingNBodySystem: NBodySystem): BigInt = {
    val (cycleOne, cycleTwo, cycleThree) = returnTimeCycles(startingNBodySystem) match {
      case (Some(a), Some(b), Some(c)) => (BigInt(a), BigInt(b), BigInt(c))
    }

    val factorsOne = LazyPrimeFactors.factors(cycleOne)
    val factorsTwo = LazyPrimeFactors.factors(cycleTwo)
    val factorsThree = LazyPrimeFactors.factors(cycleThree)
    val set = (factorsOne ++ factorsTwo ++ factorsThree).toSet.toSeq
      .groupBy((p: (BigInt, Int)) => p._1).toSeq
      .map(l => (l._1, l._2.max))

    set.map(p => p._1.pow(p._2._2)).product
  }

  private def returnTimeOfCycleOnAxis(returnTime: Option[Int], currentTime: Int, nbodyOne: NBodySystem, nbodyTwo: NBodySystem, axisChoice: ThreeVector => Int): Option[Int] = {
    val allBodiesAreTheSameOnTheAxis = (nbodyOne.system ++ nbodyTwo.system).groupBy(_.label).toSeq.forall{l => l._2 match {
      case Seq(bodyOne, bodyTwo) => areTwoBodiesTheSamePositionAndVelocityOnAxis(bodyOne, bodyTwo, axisChoice)
    }}

    if(returnTime.isEmpty && allBodiesAreTheSameOnTheAxis){
      Some(currentTime)
    } else {
      returnTime
    }
  }

  private def areTwoBodiesTheSamePositionAndVelocityOnAxis(bodyOne: SystemBody, bodyTwo: SystemBody, axisChoice: ThreeVector => Int): Boolean =
    axisChoice(bodyOne.position) == axisChoice(bodyTwo.position) && axisChoice(bodyOne.velocity) == axisChoice(bodyTwo.velocity)

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


