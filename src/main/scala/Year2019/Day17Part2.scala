package Year2019

import Year2019.ProgrammeOperations.vectorProgrammeToMap
import models.Position

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

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
  println(s"***************** instructions: $instructions")
 /////////////
 val sourceCode = Source.fromResource("2019/day17_2")
   .getLines()
   .toList
   .head
   .split(",")
   .toVector
   .map(n => BigInt(n.toInt))

  val inputQueue = new mutable.Queue[BigInt]()
  val outputQueue = new mutable.Queue[BigInt]()


  val startProgramme = IntCodeProgramme(programme = vectorProgrammeToMap(sourceCode),
      inputQueue = inputQueue,
      outputQueue = outputQueue)


  val endProgramme = startProgramme.runProgramme()(RunningSettings("trial", debugOutput = false))

  //add programme for dust sweeping
  inputQueue.enqueueAll(instructions.map(n => BigInt(n)))

  val outputs = endProgramme.outputQueue.dequeueAll(_ => true)

  val outputAscii = outputs.map(_.intValue.toChar)

  println(s"---------------------------")
  println(outputAscii)
  println(s"---------------------------")

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
      //println(s"*********** possible moves: $possibleMoves")
      val position  = if(possibleMoves.size == 1) possibleMoves.head else throw new Exception("There is not one unique move!")
      //turn to the right direction
      val newDirection = neighbours.map(x => (x, sweeper.location + x)).filter(l => l._2 == position).head._1
      val newOrientation = Orientation.fromVector(newDirection)
      val turnDirection = Orientation.turning(sweeper.orientation, newOrientation)
      // move as far as possible
      val (noOfMoves, endPosition, leftOverScaffolding) = move(newDirection, sweeper.location, 0, scaffolding)
      //println(s"********************** move part: ${(turnDirection, noOfMoves)}")
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
    def go(noOfTokens: Int, matchPath: Seq[String], matchString: Seq[String]): String = {
        if(matchPath.drop(noOfTokens).containsSlice(matchString)) matchString.mkString(",")
        else if(noOfTokens == 0) ""
        else go(noOfTokens - 2, matchPath, matchString.take(noOfTokens - 2))
      }

    @tailrec
    def mopUp[A](a: Seq[A], b: Seq[A], tokenPath: Seq[A], c: Seq[A] = Seq.empty): Seq[A] = {
      //println(s"** a: $a, b: $b, c: $c, token: $tokenPath")
      if(tokenPath.isEmpty) c
      else if(tokenPath.startsWith(a)) {
        if(c.nonEmpty) c
        else mopUp(a, b, tokenPath.drop(a.length), c)
      } else if(tokenPath.startsWith(b)) {
        if(c.nonEmpty) c
        else mopUp(a, b, tokenPath.drop(b.length), c)
      } else mopUp(a, b, tokenPath.drop(1), c :+ tokenPath.head)
    }

    @tailrec
    def reducePath[A](a: Seq[A], b: Seq[A], c: Seq[A], tokenPath: Seq[A], route: Seq[String] = Seq.empty): Option[Seq[String]] = {
      if(tokenPath.isEmpty) Some(route)
      else if(tokenPath.startsWith(a)) {
        reducePath(a, b, c, tokenPath.drop(a.length), route :+ "A")
      } else if(tokenPath.startsWith(b)) {
        reducePath(a, b, c, tokenPath.drop(b.length), route :+ "B")
      } else if(tokenPath.startsWith(c)) {
        reducePath(a, b, c, tokenPath.drop(c.length), route :+ "C")
      } else None
    }

    val tokenisedPath = path.split(",").sliding(2, 2).map {
      case Array(x, y) => (x,y)
    }.toSeq

    println(s"******************* $tokenisedPath")

    val encodings = for {
      aLength <- (1 to 5)
      bLength <- (1 to 5)
      a = tokenisedPath.take(aLength)
      b = tokenisedPath.slice(aLength, aLength + bLength)
      c = mopUp(a, b, tokenisedPath)
      route = reducePath(a, b, c, tokenisedPath)
      if(route.isDefined)
      //each expression needs to be less than 20 chars
      //that means 5 pairs (R,4) with commas between pairs -> 5*3 + 4
      if(a.length <= 5 && b.length <= 5 && c.length <= 5 && route.get.length <= 10)
    } yield (
      a.map(l => s"${l._1},${l._2}").mkString(","),
      b.map(l => s"${l._1},${l._2}").mkString(","),
      c.map(l => s"${l._1},${l._2}").mkString(","),
      route.get.mkString(","))


    println(s"*****************There are ${encodings.length} encodings")
    println(s"*****************encodings: $encodings")

    Record(encodings.head._4, encodings.head._1, encodings.head._2, encodings.head._3)
  }
}