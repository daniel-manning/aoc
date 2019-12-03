package Year2019

import scala.io.Source

object Day01 extends App {

  def calculateFuelForModuleMass(moduleMass:Int):Int = Math.floor(moduleMass/3).toInt - 2

  val moduleMasses = Source.fromResource("day01_2019_input")
    .getLines().toList
    .map(_.toInt)

  val result = moduleMasses.map(calculateFuelForModuleMass).sum

  println(s"Total fuel needed for modules: $result")

  def totalFuelAllIn(moduleMass:Int):Int ={
    val fuelForMass = calculateFuelForModuleMass(moduleMass)
    val fuelDeltas = Seq.unfold(fuelForMass){
      fuel =>
        val updateForFuel = calculateFuelForModuleMass(fuel)
        if(updateForFuel <= 0) None else Some((updateForFuel, updateForFuel))
    }

    fuelForMass + fuelDeltas.sum
  }

  def totalFuelAllIn(moduleMasses:Seq[Int]):Int =
    moduleMasses.map(totalFuelAllIn).sum

  val totalFuelForInput = totalFuelAllIn(moduleMasses)
  println(s"With all deltas considered we need for modules: $totalFuelForInput")
}
