package Year2019

import scala.io.Source

object Day16 extends App {

  val sequence = Source.fromResource("2019/day16")
    .getLines()
    .toList.head

  val result = FFT.apply100PhasesAndKeep8MostSignificant(FFT.stringToSequence(sequence))

  println(s"After applying 100 phases and keeping the 8 most significant figures: $result")

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


  def stringToSequence(sequence:String): Seq[Int] =
    sequence.map(_.toInt - 48)

  def applyPatternToPhase(phase:Seq[Int], patten:Seq[Int]): Int = {
    val multiplier = Math.ceil(phase.length.toDouble / patten.length.toDouble).toInt
    phase
      .zip(SeqMultiplier.times(patten, multiplier))
      .map(l => l._1 * l._2)
      .sum
  }

  def considerPatternForDigit(pattern: Seq[Int], digit: Int): Seq[Int] =
    pattern.flatMap(i => SeqMultiplier.times(Seq(i), digit))

  def applyPhase(sequence: Seq[Int]): Seq[Int] = {
    (1 to sequence.length).map {
      n =>
        val patternForDigit = considerPatternForDigit(pattern, n)
        val multiplier = Math.ceil(sequence.length.toDouble / patternForDigit.length.toDouble).toInt
        val resultPattern = SeqMultiplier.times(patternForDigit, multiplier + 1).tail
        //        println(s"sequence: $sequence")
        //        println(s"resultPattern: $resultPattern")
        keepLeastSignificantDigit(applyPatternToPhase(sequence, resultPattern))
    }
  }

  def applyForPhases(sequence: Seq[Int], noOfPhases: Int): Seq[Int] = {

    @scala.annotation.tailrec
    def go(n:Int, seq: Seq[Int]): Seq[Int] = {
      if(n <= 0) seq
      else go(n - 1, applyPhase(seq))
    }

    go(noOfPhases, sequence)
  }

  def apply100PhasesAndKeep8MostSignificant(sequence: Seq[Int]): Seq[Int] =
    applyForPhases(sequence, 100).take(8)


//After applying 100 phases and keeping the 8 most significant figures: Vector(4, 5, 8, 3, 4, 2, 7, 2)
  // 3m
}