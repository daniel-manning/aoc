package Year2019

object Day16 {



}

object SeqMultiplier {
  def times[A](sequence: Seq[A], multiplier: Int): Seq[A] = (1 to multiplier).foldRight(Seq.empty[A]){
    (_, b) => b ++ sequence
  }
}


object FFT {
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

}