package Year2019

case class TwoVector(x:Int, y: Int){

  def dotProduct(that: TwoVector): Int =
    this.x*that.x + this.y*that.y

  def norm: Double = Math.sqrt(x*x + y*y)
}

object TwoVector {

  //this only works against an axis vector
  def calculateAngle(vecOne:TwoVector, vecTwo: TwoVector): Double = {
    val result = Math.acos(vecOne.dotProduct(vecTwo) / (vecOne.norm * vecTwo.norm))

    if(vecOne.x < 0) 2*Math.PI - result
    else result
  }
}
