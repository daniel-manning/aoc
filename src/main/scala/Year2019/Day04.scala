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

  def foldUpRuns(sequence:Seq[Int]):Seq[(Int, Int)] =
    sequence.tail.foldLeft(Seq((sequence.head, 1))){
      (a,b) =>
        val first = a.head
        if(b == first._1){
          (b, first._2 + 1) +: a.tail
        } else {
          (b, 1) +: a
        }
    }.reverse

  def hasTwoAdjacentNumbersTheSameNotALongerRun(sequence:Seq[Int]):Boolean =
    foldUpRuns(sequence).exists(_._2 == 2)

  val result = for {
    a <- 165432 to 707912
    if(isMonotonicSequence(constructSequence(a)))
    if(hasTwoAdjacentNumbersTheSame(constructSequence(a)))
  } yield a

  println(s"There are ${result.length} numbers that match the conditions in the range")

  val resultTwo = for {
    a <- 165432 to 707912
    if(isMonotonicSequence(constructSequence(a)))
    if(hasTwoAdjacentNumbersTheSameNotALongerRun(constructSequence(a)))
  } yield a

  println(s"There are ${resultTwo.length} numbers that match the NEW conditions in the range")
}
