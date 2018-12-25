package Day12

import org.scalatest.{Matchers, WordSpec}

class Day12Spec extends WordSpec with Matchers{

  val initalState = "#..#.#..##......###...###"
  val ruleSet: Map[String, Char] = Map(
    ("...##", '#'),
    ("..#..", '#'),
    (".#...", '#'),
    (".#.#.", '#'),
    (".#.##", '#'),
    (".##..", '#'),
    (".####", '#'),
    ("#.#.#", '#'),
    ("#.###", '#'),
    ("##.#.", '#'),
    ("##.##", '#'),
    ("###..", '#'),
    ("###.#", '#'),
    ("####.",'#')
  )

  "Cellular Automata" should {
    "evolve a ruleset for one generation" in {

      val result = List("#..#.#..##......###...###",
                        "#...#....#.....#..#..#..#")

      CellularAutomata.evolve(1, ruleSet, initalState) shouldBe result

    }

    "extend beyond it's initial state size as approptiate" in {

      val result = List(
        "#..#.#..##......###...###",
        "#...#....#.....#..#..#..#",
        "##..##...##....#..#..#..##")

      CellularAutomata.evolve(2, ruleSet, initalState) shouldBe result
    }

    "evolve correctly to 20 generations" in {

      val result = "#....##....#####...#######....#.#..##"

      CellularAutomata.evolve(20, ruleSet, initalState).last shouldBe result
    }

    "count the plant pots after a specific generation" ignore {

      CellularAutomata.countPotPositions(20, ruleSet, initalState) shouldBe 325

    }
  }



}
