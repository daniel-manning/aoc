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

      val state = CellularAutomata.evolve(1, ruleSet, initalState)

      state.map(CellularAutomata.flattenToString) shouldBe result

    }

    "evolve a ruleset for one generation with indexes" in {

      val result = List(List(('#',0),('.',1),('.',2),('#',3),('.',4),('#',5),('.',6),('.',7),('#',8),('#',9),('.',10),
        ('.',11),('.',12),('.',13),('.',14),('.',15),('#',16),('#',17),('#',18),('.',19),('.',20),('.',21),('#',22),('#',23),('#',24)),
        List(('#',0),('.',1),('.',2),('.',3),('#',4),('.',5),('.',6),('.',7),('.',8),('#',9),('.',10),
          ('.',11),('.',12),('.',13),('.',14),('#',15),('.',16),('.',17),('#',18),('.',19),('.',20),('#',21),('.',22),('.',23),('#',24)))

      val state = CellularAutomata.evolve(1, ruleSet, initalState)

      state shouldBe result

    }

    "extend beyond it's initial state size as approptiate" in {

      val result = List(
        "#..#.#..##......###...###",
        "#...#....#.....#..#..#..#",
        "##..##...##....#..#..#..##")

      val states = CellularAutomata.evolve(2, ruleSet, initalState)

      states.map(CellularAutomata.flattenToString) shouldBe result
    }

    "evolve correctly to 20 generations" in {

      val result = "#....##....#####...#######....#.#..##"

      val state = CellularAutomata.evolve(20, ruleSet, initalState).last

      CellularAutomata.flattenToString(state) shouldBe result
    }

    "count the plant pots after a specific generation" in {

      CellularAutomata.countPotPositions(20, ruleSet, initalState) shouldBe 325

    }
  }



}
