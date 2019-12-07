package Year2018

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
  val rectangles = Source.fromResource("2018/day03_input").getLines.toList.map(Rectangle.stringToRectangle)
  val areaOfOverlap = Overlap.calculateOverlap(rectangles)
  println(s"The overlap area is $areaOfOverlap")
}


object Overlap {

  def compare(rectangles:List[Rectangle]):List[Rectangle] = {
    if(rectangles.isEmpty){
      List[Rectangle]()
    }else{
      //println(s"intervals: ${intervals.tail.map(interval => intersectInterval(intervals.head, interval)).filter(_.isDefined)}")
      val others = rectangles.tail.map( rectangle => rectangles.head.intersect(rectangle)).filter(_.isDefined).map(_.get)
      others.distinct ++  compare(rectangles.tail).distinct
    }
  }

  def inclusionExclusion(shapes:List[Rectangle], multiplier:Int = 1):List[(Int, Int)] = {
    if(shapes.isEmpty){
      List[(Int, Int)]()
    }else if(shapes.size == 1){
      List((multiplier, area(shapes.head)))
    }else{
      (multiplier, shapes.map(area).sum) :: inclusionExclusion(compare(shapes), -1*multiplier)
    }
  }

  def area(rectangle: Rectangle):Int ={
    rectangle.width*rectangle.height
  }

  def calculateOverlap(shapes:List[Rectangle]):Int = {


    val overlaps = inclusionExclusion(compare(shapes))
    val area = overlaps.foldRight(0){(a,b) => b + a._1*a._2}

    area
  }

}




