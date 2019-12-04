package Year2019

object Day04 extends App {

  def constructSequence(sequence:String):Seq[Int] =
    sequence.split(",").toList.map(_.toInt)

  def constructSequence(number:Int):Seq[Int] =
    number.toString.split("").map(_.toInt)

  def isMonotonicSequence(sequence:Seq[Int]):Boolean =
    sequence.sliding(2).forall(p => if(p.length >= 2) p.head <= p.drop(1).head else true)

  def hasTwoAdjacentNumbersTheSame(sequence:Seq[Int]):Boolean =
    sequence.sliding(2).exists(p => if(p.length >= 2) p.head == p.drop(1).head else false)

  val result = for {
    a <- 165432 to 707912
    if(isMonotonicSequence(constructSequence(a)))
    if(hasTwoAdjacentNumbersTheSame(constructSequence(a)))
  } yield a

  println(s"There are ${result.length} numbers that match the conditions in the range")
}
