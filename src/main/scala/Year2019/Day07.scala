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


}
