package utils

import scala.collection.mutable

class PrimeStream {

  var table:mutable.Map[BigInt,List[BigInt]] = mutable.Map.empty[BigInt,List[BigInt]]

  def sieve(xs:Iterator[BigInt]):LazyList[BigInt] = {
    lazy val m = xs.next()
    table.get(m) match {
      case None => {
        insert(table, (m * m), List(m));
        m #:: sieve(xs)
      }
      case Some(facts) => {
        table = facts.foldLeft(delete(table, m))((table: mutable.Map[BigInt, List[BigInt]], prime: BigInt) => reinsert(m)(prime, table))
        sieve(xs)
      }
    }
  }

  def insert[K,V](m:mutable.Map[K,V], k: K, v:V) = m += ((k, v))


  def insertWith[K,V](m:mutable.Map[K,V], k: K, v: V, f: (V, V) => V): mutable.Map[K, V] = {
    m.updated(k, m.get(k).map(f(v, _)).getOrElse(v))
  }

  def delete(m:mutable.Map[BigInt,List[BigInt]], k: BigInt):mutable.Map[BigInt, List[BigInt]] = m - k

  def reinsert(x:BigInt)(prime:BigInt, table:mutable.Map[BigInt,List[BigInt]]):mutable.Map[BigInt,List[BigInt]] = insertWith(table, (x+prime), List(prime), (y:List[BigInt],z:List[BigInt]) => (y++z))

}
