package Year2019

import models.Position

import scala.annotation.tailrec
import scala.concurrent.Future

sealed trait SearchResult
case object SearchSuccess extends SearchResult
case class SearchFailure(availableNextSteps: Seq[(Orientation, CraftTile)]) extends SearchResult

sealed trait MazeTree
case object MazeDeadEnd extends MazeTree
case class MazeBranch(length: Int, orientation: Orientation, left: MazeTree, middle: MazeTree, right: MazeTree) extends MazeTree

object MazeSolverMine {

  def searchFrom(maze: Set[CraftTile], position: Position)(implicit ex1: Future[IntCodeProgramme], queues: Queues): SearchResult = {

    //println(s"searching from position: $position")
    //have we found the exit?
    if(maze.find(_.position == position).exists(_.isInstanceOf[OxygenUnit])) {
      SearchSuccess
    } else {
      //we need to explore a little more
      //TODO: This bit needs to be better and prioritise unexplored tiled (which won't be in the set) otherwise we'll loop
      val unvisited: Seq[(Orientation, CraftTile)] = Seq(North, South, East, West).map {
        (d: Orientation) =>
          val p = position + d.vector
          maze.find(_.position == p).map(t => (d, t)).getOrElse((d, Unexplored(p)))
      }.filterNot {
        t =>
          val tile = t._2
          tile.isInstanceOf[CraftWall] || tile.isInstanceOf[DeadEnd]
      }

      println(unvisited)

      SearchFailure(unvisited)
    }
  }

  def closeOffDeadEnds(maze: Set[CraftTile], position: Position, availableDirections: Set[(Orientation, CraftTile)]): Set[CraftTile] = {
    if(availableDirections.size < 2 && position != Position(0, 0)){
      //change tile you're standing on to a dead end
      maze.find(_.position == position)
        .map(t => maze - t + DeadEnd(position))
        .getOrElse(maze + DeadEnd(position))
    } else maze
  }

  private def impassable(maze: Set[CraftTile], position: Position): Boolean =
    (maze.find(_.position == position)) match {
      case Some(_: CraftWall) | Some(_: Unexplored) | None  => true
      case _ => false
    }

  def convertMazeToTree(maze: Set[CraftTile], startingLocation: Position, startingDirection: Orientation): MazeTree =
    {
      //println("------------------")
      //println(s"convertMazeToTree: $startingLocation ~ $startingDirection")

        val lengthOfRun: Int = findNextBranchPoint(maze, startingLocation, startingDirection, count = 0)
        //println(s"length of run: $lengthOfRun")
        val leftDirection: Orientation = Orientation.changeDirection(startingDirection, LeftTurn)
        val leftStartingPosition: Position = startingLocation + (startingDirection.vector * (lengthOfRun-1)) + leftDirection.vector
        val rightDirection: Orientation = Orientation.changeDirection(startingDirection, RightTurn)
        val rightStartingPosition: Position = startingLocation + (startingDirection.vector * (lengthOfRun-1)) + rightDirection.vector
        val middleStartingPosition: Position = startingLocation + (startingDirection.vector * (lengthOfRun-1)) + startingDirection.vector


        //println(s"going to look from $startingLocation in directions: $leftDirection, $startingDirection, $rightDirection")
        //println(s"moving on to look for walls at: $leftStartingPosition, $middleStartingPosition, $rightStartingPosition")
        //println("------------------")

        MazeBranch(lengthOfRun, startingDirection,
          if(impassable(maze, leftStartingPosition)) MazeDeadEnd else convertMazeToTree(maze, leftStartingPosition, leftDirection),
          if(impassable(maze, middleStartingPosition)) MazeDeadEnd else convertMazeToTree(maze, middleStartingPosition, startingDirection),
          if(impassable(maze, rightStartingPosition)) MazeDeadEnd else convertMazeToTree(maze, rightStartingPosition, rightDirection)
        )
    }

  @tailrec
  private def findNextBranchPoint(maze: Set[CraftTile], startingLocation: Position, startingDirection: Orientation, count: Int): Int = {
    val leftDirection: Orientation = Orientation.changeDirection(startingDirection, LeftTurn)
    val rightDirection: Orientation = Orientation.changeDirection(startingDirection, RightTurn)

    val availableDirections = Seq(startingLocation + leftDirection.vector,  startingLocation + rightDirection.vector)
      .map(p => maze.find(_.position == p))
      .filterNot(oct => oct.exists(ct => ct.isInstanceOf[CraftWall] || ct.isInstanceOf[Unexplored]))
      .length

    val inForwardDirection = Seq(startingLocation + startingDirection.vector).map(p => maze.find(_.position == p))
      .filterNot(oct => oct.exists(ct => ct.isInstanceOf[CraftWall] || ct.isInstanceOf[Unexplored]))
      .length

    if(inForwardDirection == 0 || availableDirections > 0) {
      count + 1
    } else {
      findNextBranchPoint(maze, startingLocation + startingDirection.vector, startingDirection, count + 1)
    }
  }

  def depthOfMazeTree(mazeBranch: MazeTree): Int =
    mazeBranch match {
      case _: MazeDeadEnd.type => 0
      case branch: MazeBranch => branch.length + Seq(depthOfMazeTree(branch.left), depthOfMazeTree(branch.middle), depthOfMazeTree(branch.right)).max
    }

}

