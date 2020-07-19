package Year2019

import scala.concurrent.Future

sealed trait SearchResult
case object SearchSuccess extends SearchResult
case class SearchFailure(availableNextSteps: Seq[(Orientation, CraftTile)]) extends SearchResult

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
          val (dx, dy) = d.vector
          val p = Position(position.x + dx, position.y + dy)
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

}

