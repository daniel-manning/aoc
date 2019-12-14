package Year2019

case class Rational(num: Int, denom: Int){
  def isInteger: Boolean = denom == 1

  def lowestTerms: Rational = {
    val multiplier = denom.sign.toInt
    val gcd = BigInt(num).gcd(denom).toInt

    if(multiplier != 0 ) Rational(multiplier * num / gcd, multiplier * denom / gcd)
    else Rational(num / gcd, denom / gcd)
  }

  def +(that: Rational): Rational =
    Rational(this.num*that.denom + that.num*this.denom, this.denom*that.denom).lowestTerms

  def *(that: Rational): Rational =
    Rational(this.num*that.num, this.denom*that.denom).lowestTerms

  def floor: Int = Math.floor(num.toDouble / denom).toInt

  def toBigDecimal: BigDecimal = if(denom != 0){ BigDecimal(num)/BigDecimal(denom) } else { BigDecimal(num.sign * Double.MaxValue) }
}

object Rational {
  implicit def fromInt(value: Int): Rational = Rational(value, 1)
}
