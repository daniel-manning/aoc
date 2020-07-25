package Year2019

import models.Position
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MazeSolverSpec extends AnyWordSpec with Matchers {

  "depth of tree" should {
    "calculate depth correctly" in {
      val tree = MazeBranch(1, North,
        MazeDeadEnd,
        MazeBranch(3, North, MazeBranch(2, West, MazeDeadEnd, MazeDeadEnd, MazeDeadEnd), MazeDeadEnd, MazeBranch(5, East, MazeDeadEnd, MazeDeadEnd, MazeDeadEnd)),
        MazeBranch(2, East, MazeDeadEnd, MazeBranch(7, North, MazeDeadEnd, MazeDeadEnd, MazeDeadEnd), MazeDeadEnd))

      MazeSolverMine.depthOfMazeTree(tree) shouldBe 10
    }

    "calculate two branches correctly" in {
      val tree = MazeBranch(0, North,
        MazeBranch(1, East, MazeDeadEnd, MazeDeadEnd, MazeBranch(2, North, MazeDeadEnd, MazeDeadEnd, MazeBranch(1,East, MazeDeadEnd, MazeDeadEnd, MazeDeadEnd))),
        MazeDeadEnd,
        MazeBranch(1, West, MazeBranch(1, North, MazeDeadEnd, MazeDeadEnd, MazeBranch(1, West, MazeDeadEnd, MazeDeadEnd, MazeDeadEnd)), MazeDeadEnd, MazeDeadEnd)
      )

      MazeSolverMine.depthOfMazeTree(tree) shouldBe 4
    }
  }

  "maze to tree" should {
    "construct a tree correctly" in {
      val maze: Set[CraftTile] = Set(
        CraftWall(Position(-1,3)), CraftWall(Position(0,3)),
        CraftWall(Position(-2,2)), EmptySpace(Position(-1,2)), EmptySpace(Position(0,2)), CraftWall(Position(1,2)), CraftWall(Position(2,2)),
        CraftWall(Position(-2,1)), EmptySpace(Position(-1,1)), CraftWall(Position(0,1)), EmptySpace(Position(1,1)), EmptySpace(Position(2,1)), CraftWall(Position(3,1)),
        CraftWall(Position(-2,0)), EmptySpace(Position(-1,0)), OxygenUnit(Position(0,0)), EmptySpace(Position(1,0)), CraftWall(Position(2,0)),
        CraftWall(Position(-1,-1)), CraftWall(Position(0,-1)), CraftWall(Position(1,-1))
      )

      val tree = MazeBranch(0, North,
        MazeBranch(1, East, MazeDeadEnd, MazeDeadEnd, MazeBranch(2, North, MazeDeadEnd, MazeDeadEnd, MazeBranch(1,East, MazeDeadEnd, MazeDeadEnd, MazeDeadEnd))),
        MazeDeadEnd,
        MazeBranch(1, West, MazeBranch(1, North, MazeDeadEnd, MazeDeadEnd, MazeBranch(1, West, MazeDeadEnd, MazeDeadEnd, MazeDeadEnd)), MazeDeadEnd, MazeDeadEnd)
      )

      MazeSolverMine.convertMazeToTree(maze, OxygenUnit(Position(0,0)), North) shouldBe tree
    }
  }

}
