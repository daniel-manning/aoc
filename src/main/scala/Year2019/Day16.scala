package Year2019

import Year2019.FFT.{considerPatternForDigit, pattern}

import scala.io.Source

object Day16 extends App {

  val sequence = Source.fromResource("2019/day16")
    .getLines()
    .toList.head

  val result = FFT.apply100PhasesAndKeep8MostSignificant(FFT.stringToSequence(sequence))

  println(s"After applying 100 phases and keeping the 8 most significant figures: $result")

  //println(s"Sequence length: ${sequence.length}")

}

object SeqMultiplier {
  def times[A](sequence: Seq[A], multiplier: Int): Seq[A] = (1 to multiplier).foldRight(Seq.empty[A]){
    (_, b) => b ++ sequence
  }
}


object FFT {
  val pattern = Seq(0,1,0,-1)

  def keepLeastSignificantDigit(value: Int):Int =
    value.toString.last.toInt - 48


  def stringToSequence(sequence:String): Vector[Int] =
    sequence.map(_.toInt - 48).toVector

  def applyPatternToPhase(phase:Vector[Int], n: Int): Int = {
    val length = phase.length
    val pluses = (1 to length).flatMap(x => (1 to n).map(l => (n * (4*x - 3) + l - 2))).takeWhile(_ < length)
    val minuses = (1 to length).flatMap(x => (1 to n).map(l => (n * (4*x - 1) + l - 2))).takeWhile(_ < length)
/*    println(s"sums: ${pluses.map(phase)}")
    println(s"minuses: ${minuses.map(phase)}")*/

    pluses.map(phase).sum - minuses.map(phase).sum
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


//After applying 100 phases and keeping the 8 most significant figures: Vector(4, 5, 8, 3, 4, 2, 7, 2)
  // 3m
}