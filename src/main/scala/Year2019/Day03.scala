package Year2019

import scala.io.Source

object Day03 extends App {

  def intersectionPointsOfTwoLines(first: Line, second: Line): Int = {
    first.points.intersect(second.points).tail.map{
      a => Math.abs(a._1) + Math.abs(a._2)
    }.min
  }

  def intersectionPointsOfTwoLinesConsideringTiming(first: Line, second: Line): Int = {
    val firstLineWithTiming = first.points.zipWithIndex.tail
    val secondLineWithTiming = second.points.zipWithIndex.tail
    val intersections = first.points.intersect(second.points).tail
    val filteredFirstCrossing = firstLineWithTiming.filter(x => intersections.contains(x._1))
    val filteredSecondCrossing = secondLineWithTiming.filter(x => intersections.contains(x._1))
    val intersectionsWithTiming = filteredFirstCrossing ++ filteredSecondCrossing

    intersectionsWithTiming.groupBy(_._1)
      .map(x => (x._1, x._2.map(_._2).sum))
      .minBy(_._2)
      ._2
  }

  val lines = Source.fromResource("2019/day03")
    .getLines().toList
    .map(Line.constructFromDirections)

  val distance = intersectionPointsOfTwoLines(lines.head, lines.drop(1).head)

  println(s"Closest crossing of defined lines is $distance")

  val stepDistance = intersectionPointsOfTwoLinesConsideringTiming(lines.head, lines.drop(1).head)
  println(s"Fastest crossing of defined lines is $stepDistance")

}


case class Line(points: Seq[(Int, Int)]){

  def add(command:String): Line = {
    val direction = command.head
    val length = command.drop(1).toInt
    val (x, y) = points.last

    val extension = direction match {
        case 'U' => (1 to length).map(i => (x, y + i))
        case 'D' => (1 to length).map(i => (x, y - i))
        case 'L' => (1 to length).map(i => (x - i, y))
        case 'R' => (1 to length).map(i => (x + i, y))
      }

    Line(points ++ extension)
  }

}

object Line {
  def constructFromDirections(directions: String): Line = {
    val commands: Seq[String] = directions.split(",")
    commands.foldLeft(Line(Seq((0, 0)))){
      (line, command) => line.add(command)
    }
  }
}