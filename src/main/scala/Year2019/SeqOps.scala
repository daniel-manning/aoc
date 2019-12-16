package Year2019

object SeqOps {

  def crossDifferentPairs[A](sequence: Seq[A]): Seq[(A,A)] = {
    @scala.annotation.tailrec
    def go(result: Seq[(A,A)], rest: Seq[A]): Seq[(A,A)] = {
      if (rest.isEmpty) result
      else go(result ++ rest.tail.map(a => (rest.head, a)), rest.tail)
    }

    go(Seq.empty, sequence)
  }


}
