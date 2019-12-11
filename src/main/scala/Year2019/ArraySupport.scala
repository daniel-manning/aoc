package Year2019

object ArraySupport {

  def reconstructArray[A](sequence: Seq[(Position, A)]): Seq[Seq[A]] =
    sequence.groupBy(_._1.y).toSeq.sortBy(l => l._1)
       .map(p => p._2.sortBy(_._1.x).map(_._2))

}
