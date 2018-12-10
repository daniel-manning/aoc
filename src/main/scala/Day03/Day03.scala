package Day03

import scala.io.Source

case class Rectangle(id:String, origin:(Int,Int), height:Int, width:Int){

  def intersect(that:Rectangle):Option[Rectangle] = {
   if( this.origin._1 >= that.origin._1 + that.width || that.origin._1 >= this.origin._1 + this.width ||
     this.origin._2 >= that.origin._2 + that.height || that.origin._2 >= this.origin._2 + this.height ){
     None
   }else{
     val start = (Math.max(this.origin._1, that.origin._1), Math.max(this.origin._2, that.origin._2))
     val width = Math.min(this.origin._1 + this.width, that.origin._1 + that.width) - start._1
     val height = Math.min(this.origin._2 + this.height, that.origin._2 + that.height) - start._2

     Some(Rectangle("", start, width , height))
   }
  }

}

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

object Day03 extends App{
  val rectangles = Source.fromResource("day03_input").getLines.toList.map(Rectangle.stringToRectangle)
  val areaOfOverlap = Overlap.calculateOverlap(rectangles)
}


object Overlap {

  def calculateOverlap(shapes:List[Rectangle]):Int = {
    def compare(rectangles:List[Rectangle]):List[Rectangle] = {
      if(rectangles.isEmpty){
        List[Rectangle]()
      }else{
        //println(s"intervals: ${intervals.tail.map(interval => intersectInterval(intervals.head, interval)).filter(_.isDefined)}")
        val others = rectangles.tail.map( rectangle => rectangles.head.intersect(rectangle)).filter(_.isDefined).map(_.get)
        others.distinct ++  compare(rectangles.tail).distinct
      }
    }

    val overlaps = compare(shapes)
    println(s"overlaps: ${overlaps}")

    val area = overlaps.map(r => (r.width*r.height)).sum
    println(s"Area: ${area} from ${overlaps.size} overlaps")
    val secondOrder = compare(overlaps)
    val secondOrderArea = secondOrder.map(r => (r.width*r.height)).sum
    println(s"Second Order Area: ${secondOrderArea} from ${secondOrder.size} overlaps")

    area
  }

  def pointInRectangle(point:(Double, Double), rectangle: Rectangle):Boolean = {
    println(s"point: $point rectangle: $rectangle")
    (rectangle.origin._1 <= point._1) && (point._1 <= rectangle.origin._1 + rectangle.width) &&
      (rectangle.origin._2 <= point._2) && (point._2 <= rectangle.origin._2 + rectangle.height)
  }

}




