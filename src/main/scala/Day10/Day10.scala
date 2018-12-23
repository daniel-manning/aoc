package Day10

import scala.io.{Source, StdIn}

case class Particle(position:(Int, Int), velocity:(Int, Int)){
  def nextTimeStep:Particle = {
    Particle((position._1 + velocity._1, position._2 + velocity._2), velocity)
  }
}

object Day10 extends App{
  val particlePattern = "position=<([ 0-9\\-]+),([ 0-9\\-]+)> velocity=<([ 0-9\\-]+),([ 0-9\\-]+)>".r

  val particles:List[Particle] = Source.fromResource("day10_input").getLines.toList.map{
    l => particlePattern.findFirstMatchIn(l).map( mat => Particle((mat.group(1).trim.toInt, mat.group(2).trim.toInt), (mat.group(3).trim.toInt, mat.group(4).trim.toInt))).get
  }


  keepGoing(particles, 0)

  def printIteration(particlesL:List[Particle], sec:Int):Unit = {
    val minX = particlesL.minBy(_.position._1).position._1
    val minY = particlesL.minBy(_.position._2).position._2
    val maxX = particlesL.maxBy(_.position._1).position._1
    val maxY = particlesL.maxBy(_.position._2).position._2

    println(s"($minX, $minY) to ($maxX, $maxY) at $sec seconds")
    println("**********************************************")

    (minY to maxY).toList.foldLeft(()){
      (_, y) => (minX to maxX).toList.foldLeft(()){
        (_, x) => if(particlesL.exists(p => p.position == (x, y))) print("x") else print(".")
      }; print("\n")
    }

  }

  def keepGoing(particlesL:List[Particle], sec:Int):Unit = {
    val minX = particlesL.minBy(_.position._1).position._1
    val maxX = particlesL.maxBy(_.position._1).position._1
    if(maxX - minX > 100){
      keepGoing(evolve(particlesL), sec + 1)
    }else {

      if (StdIn.readChar().toInt == 27) {
        printIteration(particlesL, sec)
        keepGoing(evolve(particlesL), sec + 1)
      }
    }
  }

  def evolve(particles:List[Particle]):List[Particle] = {
    particles.map(p => Particle((p.position._1 + p.velocity._1, p.position._2 + p.velocity._2), p.velocity))
  }
}

