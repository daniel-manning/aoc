package Year2019

import java.util.concurrent.Executors

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.io.Source
import scala.language.postfixOps

object Day07 extends App {

  //Need a threadpool greater than 5
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))

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

  val allPossibleJerryRiggedSettings = (5 to 9).permutations.toList

  val things = allPossibleJerryRiggedSettings.map(settings => Amplifiers.runJerryRiggedAmplifiers(settings, amplifierSourceCode))
  val outputsFuture = Future.sequence(things)

  val output = Await.result(outputsFuture, 4 minutes)

  println(s"Maximum output for all jerry rigged settings: ${output.max}")
}

object Amplifiers {

  def runAmplifiers(settings: Seq[Int], programmeCode:Vector[Int]):Int =
    settings.foldLeft(0){
       (previousOutput, setting) =>

         val inputChannel = new mutable.Queue[Int]().enqueue(setting, previousOutput)

         val amplifierProgramme = IntCodeProgramme(
           programme = programmeCode,
           inputQueue = inputChannel
         )

         //println(s"input stack is ${amplifierProgramme.inputStack}")

         val finalState = amplifierProgramme.runProgramme()

         finalState.outputQueue.head
     }


  def runJerryRiggedAmplifiers(settings: Seq[Int], amplifierSourceCode: Vector[Int])(implicit ec: ExecutionContext): Future[Int] = {
    val link1 = new mutable.Queue[Int]()
    val link2 = new mutable.Queue[Int]()
    val link3 = new mutable.Queue[Int]()
    val link4 = new mutable.Queue[Int]()
    val link5 = new mutable.Queue[Int]()

    //provide Settings
    link1.enqueue(settings(0))
    link2.enqueue(settings(1))
    link3.enqueue(settings(2))
    link4.enqueue(settings(3))
    link5.enqueue(settings(4))

    //start up the chain
    link1.enqueue(0)

    //setup computers
    val computerOne = IntCodeProgramme(programme = amplifierSourceCode,
      inputQueue = link1,
      outputQueue = link2)
    val computerTwo = IntCodeProgramme(programme = amplifierSourceCode,
      inputQueue = link2,
      outputQueue = link3)
    val computerThree = IntCodeProgramme(programme = amplifierSourceCode,
      inputQueue = link3,
      outputQueue = link4)
    val computerFour = IntCodeProgramme(programme = amplifierSourceCode,
      inputQueue = link4,
      outputQueue = link5)
    val computerFive = IntCodeProgramme(programme = amplifierSourceCode,
      inputQueue = link5,
      outputQueue = link1)

    //Evaluate each computer in a separate Thread
    val ex1 = Future { computerOne.runProgramme()(RunningSettings("AmplifierOne", debugOutput = false))}
    val ex2 = Future { computerTwo.runProgramme()(RunningSettings("AmplifierTwo", debugOutput = false))}
    val ex3 = Future { computerThree.runProgramme()(RunningSettings("AmplifierThree", debugOutput = false))}
    val ex4 = Future { computerFour.runProgramme()(RunningSettings("AmplifierFour", debugOutput = false))}
    val ex5 = Future { computerFive.runProgramme()(RunningSettings("AmplifierFive", debugOutput = false))}

    val futures = Future.sequence(Seq(ex1, ex2, ex3, ex4, ex5))

    futures.map(_(4).outputQueue.head)
  }
}
