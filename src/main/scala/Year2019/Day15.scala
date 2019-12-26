package Year2019

import Year2019.ProgrammeOperations.vectorProgrammeToMap

import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source

import scala.concurrent.ExecutionContext.Implicits.global

object Day15 {

  val sourceCode: Vector[BigInt] = Source.fromResource("2019/day15")
    .getLines()
    .toList
    .head
    .split(",")
    .toVector
    .map(BigInt(_))

  implicit val (ex1, queues) = RepairDroid.loadProgramme(sourceCode)



}

sealed trait DroidStatus
case object BlockedByWall extends DroidStatus
case object Moved extends DroidStatus
case object MovedToOxygenUnit extends DroidStatus


object RepairDroid {

  def loadProgramme(sourceCode: Vector[BigInt]): (Future[IntCodeProgramme], Queues) = {
    val queues = Queues(new mutable.Queue[BigInt](), new mutable.Queue[BigInt](), new mutable.Queue[BigInt]())

    val computerOne = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
      inputQueue = queues.inputQueue,
      outputQueue = queues.outputQueue,
      waitingForInputQueue = queues.waitingForInputQueue)

    val ex1 = Future {
      computerOne.runProgramme()(RunningSettings("RepairDroid", debugOutput = false))
    }

    (ex1, queues)
  }

   def setDirection(direction: Orientation, queues: Queues): Unit = {
     direction match {
       case North => queues.inputQueue.enqueue(BigInt(1))
       case South => queues.inputQueue.enqueue(BigInt(2))
       case West => queues.inputQueue.enqueue(BigInt(3))
       case East => queues.inputQueue.enqueue(BigInt(4))
     }
   }

  private def getOutputs(implicit ex1: Future[IntCodeProgramme], queues: Queues): Seq[BigInt] = {
    //block thread until programme exits or waits for Input
    while (!ex1.isCompleted && queues.waitingForInputQueue.isEmpty) {
      Thread.sleep(20)
    }

    //remove any messages
    queues.waitingForInputQueue.dequeueAll(_ => true)

    //gather outputs
    queues.outputQueue.dequeueAll(_ => true)
  }


}