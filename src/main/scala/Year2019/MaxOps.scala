package Year2019

object MaxOps {

  def maxBy[A, B : Ordering](elements: IterableOnce[A])(f: A => B): Seq[A] = {
    val maxValue = f(elements.iterator.maxBy(f))
    elements.iterator.filter(f(_) == maxValue).toList
  }

}
