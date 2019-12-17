package utils

object LazyPrimeFactors {

  lazy val primesStream:PrimeStream = new PrimeStream

  def factors(x:BigInt):List[(BigInt,Int)] = {
    def go(x:BigInt, primeList:LazyList[BigInt]):List[(BigInt,Int)] = {
      //println(s"x: $x with primeList: $primeList")
      if(x == 1) {
        List()
      }else if(primeList.isEmpty){
        List((x,1))
      } else {
        if( x % primeList.head == 0) {
          val (powers, remainder) = rollUpPowers(x / primeList.head, primeList.head, (primeList.head, 1))
          powers :: go(remainder, primeList.tail)
        }else if(primeList.head*primeList.head > x ){
          List((x,1))
        }else{
          go(x, primeList.tail)
        }
      }
    }

    def rollUpPowers(x:BigInt, prime:BigInt, powers:(BigInt,Int)):((BigInt,Int),BigInt) = {
      if( x % prime == 0){
        rollUpPowers(x/prime, prime, (powers._1, (powers._2) + 1))
      }else{
        (powers, x)
      }
    }
    //println(s"First call x: $x")
    go(x, primesStream.sieve(Iterator.iterate(BigInt(2)){_+1}).takeWhile(p => {(p-1)*(p-1) <= x}))
  }

  def noOfDivisors(x:BigInt):BigInt = factors(x).foldLeft(1)((total, numPow) => total * (numPow._2 + 1))

  def totient(n:BigInt):BigInt = factors(n).map(x => (x._1, x._2-1)).foldLeft(BigInt(1))((total, numPow) => total * numPow._1.pow(numPow._2) *(numPow._1 - 1))

  def sumOfProperDivisors(n:BigInt): Int = (factors(n).map(n => (((n._1).pow(n._2 + 1))-1)/(n._1 - 1)).product - n).toInt

  def sopf(n:BigInt): BigInt = factors(n).map(n => n._1).sum

  def isPrime(n:BigInt):Boolean = factors(n) == List((n,1))

  @scala.annotation.tailrec
  def gcd(a:BigInt, b:BigInt):BigInt = if(b == 0) a else gcd(b, a % b)

  def reduce(a:BigInt, b:BigInt):(BigInt,BigInt) = {
    val hcf = gcd(a,b)
    (a/hcf, b/hcf)
  }

}