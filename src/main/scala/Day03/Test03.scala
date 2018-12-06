package Day03

case class Rectangle(id:String, origin:(Int,Int), height:Int, width:Int)

object Rectangle {

  def stringToRectangle(definition:String): Rectangle = {
    val regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)".r
    regex.findFirstMatchIn(definition).map( patternMatch =>
      Rectangle(patternMatch.group(1),
        (patternMatch.group(2).toInt, patternMatch.group(3).toInt),
        patternMatch.group(4).toInt,
        patternMatch.group(5).toInt)).get
  }
  
}

object Test03 extends App{
 val rec:Rectangle = Rectangle.stringToRectangle("#1 @ 1,3: 4x4")
  println(rec)
}


object Overlap {

  def calculateOverlap(shapes:Seq[Rectangle]):Int = {
   ???
  }

  def pointInRectangle(point:(Double, Double), rectangle: Rectangle):Boolean = {
    println(s"point: $point rectangle: $rectangle")
    (rectangle.origin._1 <= point._1) && (point._1 <= rectangle.origin._1 + rectangle.width) &&
      (rectangle.origin._2 <= point._2) && (point._2 <= rectangle.origin._2 + rectangle.height)
  }

}




