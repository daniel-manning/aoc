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

      //val result = "#...#....#.....#..#..#..#"
      val result = List(0, 4, 9, 15, 18, 21, 24)


      val state = CellularAutomata.evolve(1, ruleSet, initalState)

      state shouldBe result

    }

    "extend beyond it's initial state size as approptiate" in {

      //val result = "##..##...##....#..#..#..##"
      val result = List(0, 1,  4, 5, 9, 10, 15, 18, 21, 24, 25)

      val state = CellularAutomata.evolve(2, ruleSet, initalState)

      state shouldBe result
    }

    "evolve correctly to 20 generations" in {

      //val result = "#....##....#####...#######....#.#..##"
      val result = List(-2, 3, 4, 9, 10, 11, 12, 13, 17, 18, 19, 20, 21, 22, 23, 28, 30, 33, 34)

      val state = CellularAutomata.evolve(20, ruleSet, initalState)

      state shouldBe result
    }

    "count the plant pots after a specific generation" in {

      CellularAutomata.countPotPositions(20, ruleSet, initalState) shouldBe 325

    }
  }



}
