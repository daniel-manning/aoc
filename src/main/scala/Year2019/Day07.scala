package Year2019

import scala.collection.mutable
import scala.io.Source

object Day07 extends App {

  val amplifierSourceCode = Source.fromResource("2019/day07")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(_.toInt)

  val allPossibleSettings = (0 to 4).permutations.toList

  val amplifierOutput = allPossibleSettings.map(settings => Amplifiers.runAmplifiers(settings, amplifierSourceCode))

  println(s"Maximum output for all settings is: ${amplifierOutput.max}")

}

object Amplifiers {

  def runAmplifiers(settings: Seq[Int], programmeCode:Vector[Int]):Int =
    settings.foldLeft(0){
       (previousOutput, setting) =>

         val inputChannel = new mutable.Stack[Int]().push(previousOutput, setting)

         val amplifierProgramme = IntCodeProgramme(
           programme = programmeCode,
           inputStack = inputChannel
         )

         //println(s"input stack is ${amplifierProgramme.inputStack}")

         val finalState = amplifierProgramme.runProgramme()

         finalState.outputStack.head
     }


  def runJerryRiggedAmplifiers(settings: Seq[Int], amplifierSourceCode: Vector[Int]): Int = {
    val link1 = new mutable.Stack[Int]()
    val link2 = new mutable.Stack[Int]()
    val link3 = new mutable.Stack[Int]()
    val link4 = new mutable.Stack[Int]()
    val link5 = new mutable.Stack[Int]()

    //provide Settings
    link1.push(settings(0))
    link2.push(settings(1))
    link3.push(settings(2))
    link4.push(settings(3))
    link5.push(settings(4))

    //setup computers
    val computerOne = IntCodeProgramme(programme = amplifierSourceCode,
      inputStack = link1,
      outputStack = link2)
    val computerTwo = IntCodeProgramme(programme = amplifierSourceCode,
      inputStack = link2,
      outputStack = link3)
    val computerThree = IntCodeProgramme(programme = amplifierSourceCode,
      inputStack = link3,
      outputStack = link4)
    val computerFour = IntCodeProgramme(programme = amplifierSourceCode,
      inputStack = link4,
      outputStack = link5)
    val computerFive = IntCodeProgramme(programme = amplifierSourceCode,
      inputStack = link5,
      outputStack = link1)

    //Evaluate each computer in a separate Thread
    ???
  }
}
