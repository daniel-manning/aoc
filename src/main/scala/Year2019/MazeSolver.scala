package Year2019

import scala.concurrent.Future

sealed trait SearchResult
case object SearchSuccess extends SearchResult
case class SearchFailure(availableNextSteps: Seq[Orientation]) extends SearchResult

object MazeSolverMine {

  def searchFrom(maze: Set[CraftTile], position: Position)(implicit ex1: Future[IntCodeProgramme], queues: Queues): SearchResult = {

    println(s"searching from position: $position")
    //have we found the exit?
    if(maze.find(_.position == position).exists(_.isInstanceOf[OxygenUnit])) {
      SearchSuccess
    } else {
      //we need to explore a little more
      //TODO: This bit needs to be better and prioritise unexplored tiled (which won't be in the set) otherwise we'll loop
      val unvisited: Seq[Orientation] = Seq(North, South, East, West).filterNot {
        (d:Orientation) =>
          val (dx, dy) = d.vector
          val p = Position(position.x + dx, position.y + dy)
          maze.find(_.position == p).exists(t => t.isInstanceOf[CraftWall] || t.isInstanceOf[DeadEnd])
      }

      println(unvisited)

      SearchFailure(unvisited)
    }
  }

  def closeOffDeadEnds(maze: Set[CraftTile], position: Position, availableDirections: Set[Orientation]): Set[CraftTile] =
    if(availableDirections.size < 2){
      //change tile you're standing on to a dead end
      maze.find(_.position == position)
        .map(t => maze - t + DeadEnd(position))
        .getOrElse(maze + DeadEnd(position))
    } else maze

}


/*object MazeSolver {

  type Opt2[A] = Option[Option[A]]

  type IndexGrid = Grid[Opt2[Position]]

  type Predicate[A] = A => Boolean

  def validAndTraversable[A](isTraversable: Predicate[A], grid: Set[A], xy: Position): Boolean = {
    val (x, y) = (xy.x, xy.y)
    val xbound = grid.map(_)
    val ybound = grid(0).length
    val withinBounds = (x >= 0) && (x < xbound) && (y >= 0) && (y < ybound)
    withinBounds && isTraversable(grid(x)(y))
  }

  def getPath(grid: IndexGrid, end: Position): List[Position] = {

    @scala.annotation.tailrec
    def pathAccumulator(grid: IndexGrid, path: List[Position], xy: Position): List[Position] = {
      val (x, y) = (xy.x, xy.y)
      grid(x)(y) match {
        case Some(Some(prevXY)) => pathAccumulator(grid, xy :: path, prevXY)
        case Some(None) => xy :: path
        case None => Nil
      }
    }

    pathAccumulator(grid, Nil, end)
  }

  @scala.annotation.tailrec
  def mazeSolverLoop[A](isFinish: (Position, A) => Boolean,
                        isTraversable: Predicate[A],
                        grid: Grid[A],
                        queue: mutable.Queue[Position],
                        indexGrid: IndexGrid): List[Position] =
    if (queue.isEmpty) Nil else {
      val xy = queue.dequeue
      val (x, y) = (xy.x, xy.y)
      if (isFinish(grid(x)(y))) {
        getPath(indexGrid, xy)
      } else {
      val neighbors = List(Position(x + 1, y), Position(x, y + 1), Position(x - 1, y), Position(x, y - 1))
      val unvisited = neighbors.filter { case Position(i, j) => validAndTraversable(isTraversable, grid, Position(i, j)) && indexGrid(i)(j).isEmpty }
      for ( Position(i, j) <- unvisited ) {
        indexGrid(i)(j) = Some(Some(xy))
      }
      val updatedQueue = queue.enqueueAll(unvisited)
      mazeSolverLoop(isFinish, isTraversable, grid, updatedQueue, indexGrid)
    }
  }

  def findUnknownFinish[A](start: Position,
                           isFinish: (Position, A) => Boolean,
                           isTraversable: Predicate[A],
                           grid: Set[A]): List[Position] =
    if (validAndTraversable(isTraversable, grid, start)) {
      val (x, y) = (start.x, start.y)
      val indexGrid = Array.fill[Opt2[Position]](grid.length, grid(0).length)(None)
      indexGrid(x)(y) = Some(None)
      mazeSolverLoop(isFinish, isTraversable, grid, mutable.Queue(start), indexGrid)
    }
    else {
      Nil
    }

  def findKnownFinish[A](start: Position,
                         finish: Position,
                         isTraversable: Predicate[A],
                         grid: Grid[A]): List[Position] = findUnknownFinish(start, (xy: Position, a: A) => (xy == finish), isTraversable, grid)


  def escapeMaze[A](start: Position, isTraversable: Predicate[A], grid: Grid[A]) = {
    val xbound = grid.length
    val ybound = grid(0).length
    val boundaryPred = (xy: Position, a: A) => {
      val (x, y) = (xy.x, xy.y)
      ((x == 0) || (x == xbound - 1) || (y == 0) || (y == ybound - 1))
    }
    findUnknownFinish(start, boundaryPred, isTraversable, grid)
  }

  def escapeMazeV2[A](start: Position, isTraversable: Predicate[A], grid: Grid[A]) = {
    val xbound = grid.length
    val ybound = grid(0).length
    val boundaryPred = (xy: Position, a: A) => {
      val (x, y) = (xy.x, xy.y)
      ((x == 0) || (x == xbound - 1) || (y == 0) || (y == ybound - 1)) && (xy != start)
    }
    findUnknownFinish(start, boundaryPred, isTraversable, grid)
  }
}*/
