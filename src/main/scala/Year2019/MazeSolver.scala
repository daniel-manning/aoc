package Year2019

import models.Position

import scala.concurrent.Future

sealed trait SearchResult
case object SearchSuccess extends SearchResult
case class SearchFailure(availableNextSteps: Seq[(Orientation, CraftTile)]) extends SearchResult

sealed trait MazeTree
case object MazeDeadEnd extends MazeTree
case class MazeBranch(length: Int, orientation: Orientation, left: MazeTree, middle: MazeTree, right: MazeTree) extends MazeTree

object MazeSolverMine {

  def searchFrom(maze: Set[CraftTile], position: Position)(implicit ex1: Future[IntCodeProgramme], queues: Queues): SearchResult = {

    println(s"searching from position: $position")
    //have we found the exit?
    if(maze.find(_.position == position).exists(_.isInstanceOf[OxygenUnit])) {
      SearchSuccess
    } else {
      //we need to explore a little more
      //TODO: This bit needs to be better and prioritise unexplored tiled (which won't be in the set) otherwise we'll loop
      val unvisited: Seq[(Orientation, CraftTile)] = Seq(North, South, East, West).map {
        (d: Orientation) =>
          val p = position.add(d.vector)
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

  def convertMazeToTree(maze: Set[CraftTile], startingLocation: CraftTile, startingDirection: Orientation): MazeTree =
    {
      val lengthOfRun = ???
      val leftDirection = Orientation.changeDirection(startingDirection, LeftTurn)
      val leftStartingPosition = ??? //startingLocation.position
      val rightDirection = Orientation.changeDirection(startingDirection, RightTurn)
      val rightStartingPosition = ???
      val middleStartingPosition = ???

      MazeBranch(lengthOfRun, startingDirection,
        convertMazeToTree(maze, leftStartingPosition, leftDirection),
        convertMazeToTree(maze, middleStartingPosition, startingDirection),
        convertMazeToTree(maze, rightStartingPosition, rightDirection)
      )
    }


  def depthOfMazeTree(mazeBranch: MazeTree): Int =
    mazeBranch match {
      case _: MazeDeadEnd.type => 0
      case branch: MazeBranch => branch.length + Seq(depthOfMazeTree(branch.left), depthOfMazeTree(branch.middle), depthOfMazeTree(branch.right)).max
    }

}

