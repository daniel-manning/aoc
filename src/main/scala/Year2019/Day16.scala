package Year2019

import scala.io.Source

object Day16 extends App {

  val sequence = Source.fromResource("2019/day16")
    .getLines()
    .toList.head

  val result = FFT.apply100PhasesAndKeep8MostSignificant(FFT.stringToSequence(sequence))

  println(s"After applying 100 phases and keeping the 8 most significant figures: ${result.mkString}")

  val trueMessage = SeqMultiplier.times(sequence, 10000)
  val offset = trueMessage.take(7).mkString

  println(s"Message length is  ${trueMessage.length} with an offset of $offset")

}

object SeqMultiplier {
  def times[A](sequence: Seq[A], multiplier: Int): Seq[A] = (1 to multiplier).foldRight(Seq.empty[A]){
    (_, b) => b ++ sequence
  }
}


object FFT {
  val pattern = Seq(0,1,0,-1)

  def keepLeastSignificantDigit(value: Int): Int =
    Math.abs(value) % 10


  def stringToSequence(sequence:String): Vector[Int] =
    sequence.map(_.toInt - 48).toVector

  def applyPatternToPhase(phase:Vector[Int], n: Int): Int = {
    val pluses = phase.drop(n - 1).sliding(4*n, 4*n).toList.flatMap(_.take(n))
    val minuses = phase.drop(3 * n - 1).sliding(4*n, 4*n).toList.flatMap(_.take(n))

    pluses.sum - minuses.sum
  }

  def considerPatternForDigit(pattern: Vector[Int], digit: Int): Vector[Int] =
    pattern.flatMap(i => SeqMultiplier.times(Seq(i), digit))

  def applyPhase(sequence: Vector[Int]): Vector[Int] = {
    (1 to sequence.length).toVector.map {
      n =>
        keepLeastSignificantDigit(applyPatternToPhase(sequence, n))
    }
  }

  def applyForPhases(sequence: Vector[Int], noOfPhases: Int): Vector[Int] = {

    @scala.annotation.tailrec
    def go(n:Int, seq: Vector[Int]): Vector[Int] = {
      if(n <= 0) seq
      else go(n - 1, applyPhase(seq))
    }

    go(noOfPhases, sequence)
  }

  def apply100PhasesAndKeep8MostSignificant(sequence: Vector[Int]): Vector[Int] =
    applyForPhases(sequence, 100).take(8)

}