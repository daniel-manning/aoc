package Year2019

import models.Position

import scala.annotation.tailrec

object Day17Part2Test extends App {
val map = """#######...#####
            |#.....#...#...#
            |#.....#...#...#
            |......#...#...#
            |......#...###.#
            |......#.....#.#
            |^########...#.#
            |......#.#...#.#
            |......#########
            |........#...#..
            |....#########..
            |....#...#......
            |....#...#......
            |....#...#......
            |....#####......""".stripMargin


  val mapString = map.split('\n').toSeq.map(_.toSeq)
  val scaffold = Scaffolding.fromMap(mapString)
  val sweeper = Scaffolding.findSweeper(mapString)

  val path = Sweeper.findRoute(sweeper, scaffold)
  println(s"*************** calculatedRoute: $path")
 // val path = "R,8,R,8,R,4,R,4,R,8,L,6,L,2,R,4,R,4,R,8,R,8,R,8,L,6,L,2"
  val record = Record.fromPath(path)
  val instructions = record.toInstructions
  println(instructions)
}

object Day17Part2 extends App {
  val map = """....................############^....
              |....................#................
              |................#######..............
              |................#...#.#..............
              |................#...#.#..............
              |................#...#.#..............
              |................#.#####..............
              |................#.#.#................
              |..........#############..............
              |..........#.....#.#.#.#..............
              |..........#.....#.#.#.#..............
              |..........#.....#.#.#.#..............
              |..........#.....#####.#..............
              |..........#.......#...#..............
              |#####.....#.......#...#############..
              |#...#.....#.......#...............#..
              |#...#.....#.......#...............#..
              |#...#.....#.......#...............#..
              |#...#.....#.......#############...#..
              |#.........#...................#...#..
              |###########...................#...#..
              |..............................#...#..
              |........................#######...#..
              |........................#.........#..
              |......................###########.#..
              |......................#.#.......#.#..
              |......................#.#...#######..
              |......................#.#...#...#....
              |......................#############..
              |........................#...#...#.#..
              |........................#...#####.#..
              |........................#.........#..
              |........................#.........#..
              |........................#.........#..
              |........................#############
              |..................................#.#
              |..................................#.#
              |..................................#.#
              |..................................#.#
              |..................................#.#
              |..............................#####.#
              |..............................#.....#
              |..............................#.....#
              |..............................#.....#
              |..............................#######""".stripMargin


  val mapString = map.split('\n').toSeq.map(_.toSeq)
  val scaffold = Scaffolding.fromMap(mapString)
  val sweeper = Scaffolding.findSweeper(mapString)

  val path = Sweeper.findRoute(sweeper, scaffold)
  println(s"*************** calculatedRoute: $path")
  val record = Record.fromPath(path)
  val instructions = record.toInstructions
  println(instructions)
}

case class Sweeper(orientation: Orientation, location: Position)
object Sweeper {

  val neighbours = Seq(TwoVector(0, 1), TwoVector(1, 0), TwoVector(-1, 0), TwoVector(0, -1))

  @tailrec
  def move(direction: TwoVector, startPosition: Position, moveLength: Int, scaffolding: Scaffolding): (Int, Position, Scaffolding) = {
    if(!scaffolding.map.contains(startPosition + direction)) (moveLength, startPosition, scaffolding)
    else {
      //if position has more than 2 connects leave it because it is a self intersection
      val connections = scaffolding.map `intersect` neighbours.map(x => startPosition + x).toSet
      if(connections.size > 2)
        move(direction, startPosition + direction, moveLength + 1, scaffolding)
      else
        move(direction, startPosition + direction, moveLength + 1, Scaffolding(scaffolding.map - startPosition))
    }
  }

  @tailrec
  def calculateRoute(sweeper: Sweeper, scaffolding: Scaffolding, moves: Seq[(Char, Int)]): Seq[(Char, Int)] = {
    if(scaffolding.map.size <= 1) moves
    else {
      //find connecting point
      val possibleMoves = scaffolding.map `intersect` neighbours.map(x => sweeper.location + x).toSet
      println(s"*********** possible moves: $possibleMoves")
      val position  = if(possibleMoves.size == 1) possibleMoves.head else throw new Exception("There is not one unique move!")
      //turn to the right direction
      val newDirection = neighbours.map(x => (x, sweeper.location + x)).filter(l => l._2 == position).head._1
      val newOrientation = Orientation.fromVector(newDirection)
      val turnDirection = Orientation.turning(sweeper.orientation, newOrientation)
      // move as far as possible
      val (noOfMoves, endPosition, leftOverScaffolding) = move(newDirection, sweeper.location, 0, scaffolding)
      println(s"********************** move part: ${(turnDirection, noOfMoves)}")
      calculateRoute(Sweeper(newOrientation, endPosition),leftOverScaffolding, moves :+ (turnDirection, noOfMoves))
    }
  }

  def findRoute(sweeper: Sweeper, scaffolding: Scaffolding): String =
    calculateRoute(sweeper, scaffolding, Seq.empty).map(l => s"${l._1},${l._2}").mkString(",")

}



case class Record(movement: String, A: String, B: String, C: String) {
  def toInstructions: Seq[Int] =
    (movement.map(_.toInt) :+ 10) ++ (A.map(_.toInt) :+ 10) ++ (B.map(_.toInt) :+ 10) ++ (C.map(_.toInt) :+ 10)
}

object Record {
  def fromPath(path: String): Record = {
    //find the longest substring repeated at least once
    //21 (from the memory limit) and then down 4 each time (I think that turning and moving is a unit)
    @tailrec
    def go(length: Int, matchPath: String, matchString: String): String = {
        if(matchPath.drop(length).contains(matchString)) matchString
        else if(length == 0) ""
        else go(length - 4, matchPath, matchString.take(length - 4))
      }


    val A = go( 19, path, path.take(19))
    val newPath = path.drop(A.length + 1)
    val B = go( 19, newPath, newPath.take(19))
    val lastPath = newPath.drop(B.length + 1)
    val C = go(19, lastPath, lastPath.take(19))
    println(s"*************** A : $A")
    println(s"***************** New Path: $newPath")
    println(s"*************** B : $B")
    println(s"*************** C : $C")


    @tailrec
    def contractPath(uncontracted: String, pathParts: Seq[Char]): Seq[Char] = {
      if(uncontracted.length == 0) pathParts
      else if (uncontracted.startsWith(A)) contractPath(uncontracted.drop(A.length + 1), pathParts :+ 'A')
      else if (uncontracted.startsWith(B)) contractPath(uncontracted.drop(B.length + 1), pathParts :+ 'B')
      else if (uncontracted.startsWith(C)) contractPath(uncontracted.drop(C.length + 1), pathParts :+ 'C')
      else throw new Exception("Something went wrong!")
    }

    val reducedPath = contractPath(path, Seq.empty).mkString(",")
    println(s"****************** path: $reducedPath")

    Record(reducedPath, A, B, C)
  }
}