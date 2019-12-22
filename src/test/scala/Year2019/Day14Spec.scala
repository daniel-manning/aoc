package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day14Spec extends AnyWordSpec with Matchers {

  "Reaction Parser" should {
    "properly handle single required ingredient" in {
      val reaction = "10 ORE => 10 A"
      val result = Reaction(Seq(Ingredient(10, "ORE")), Ingredient(10, "A"))

      ReactionParser.parse(ReactionParser.reaction, reaction).get shouldBe result
    }

    "properly handle multiple requirements" in {
      val reaction = "1 A, 2 B, 3 C => 2 D"
      val result = Reaction(Seq(Ingredient(1, "A"), Ingredient(2, "B"), Ingredient(3, "C")), Ingredient(2, "D"))

      ReactionParser.parse(ReactionParser.reaction, reaction).get shouldBe result
    }
  }

  "Reactions" should {
      "find the ends of a reaction chain" in {
        val reactionInput =
          """10 ORE => 10 A
            |1 ORE => 1 B
            |7 A, 1 B => 1 C
            |7 A, 1 C => 1 D
            |7 A, 1 D => 1 E
            |7 A, 1 E => 1 FUEL""".stripMargin

        val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

        val reactionsMap = CriticalPath.turnReactionsToMap(reactions)
        CriticalPath.findEnds(reactionsMap) shouldBe Seq("FUEL")
      }
  }

  "Reaction Handler" should {
    "properly evaluate critical path" in {
      val reactionInput =
        """10 ORE => 10 A
        |1 ORE => 1 B
        |7 A, 1 B => 1 C
        |7 A, 1 C => 1 D
        |7 A, 1 D => 1 E
        |7 A, 1 E => 1 FUEL""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.minimumInput(reactions) shouldBe Ingredient(31, "ORE")
    }

    "properly decompose targets" in {
      val reactionInput =
        """10 ORE => 10 A
          |1 ORE => 1 B
          |7 A, 1 B => 1 C
          |7 A, 1 C => 1 D
          |7 A, 1 D => 1 E
          |7 A, 1 E => 1 FUEL""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.decomposeTarget("FUEL", Seq(Ingredient(1, "FUEL")), reactions) shouldBe
        Seq(Ingredient(7, "A"), Ingredient(1, "E"))
    }

    "properly Add up ingredients" in {
      val reactionInput =
        """10 ORE => 10 A
          |1 ORE => 1 B
          |7 A, 1 B => 1 C
          |7 A, 1 C => 1 D
          |7 A, 1 D => 1 E
          |7 A, 1 E => 1 FUEL""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.decomposeTarget("E", Seq(Ingredient(7, "A"), Ingredient(1, "E")), reactions) shouldBe
        Seq(Ingredient(14, "A"), Ingredient(1, "D"))
    }

    "properly handle overflow ingredients" in {
      val reactionInput =
        """5 ORE => 5 B""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.decomposeTarget("B", Seq(Ingredient(7, "A"), Ingredient(2, "B")), reactions) shouldBe
        Seq(Ingredient(7, "A"), Ingredient(5, "ORE"))
    }

    "properly handle underflow ingredients" in {
      val reactionInput =
        """5 ORE => 5 B""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.decomposeTarget("B", Seq(Ingredient(7, "A"), Ingredient(6, "B")), reactions) shouldBe
        Seq(Ingredient(7, "A"), Ingredient(10, "ORE"))
    }

    "find fuel for test case two"in {
      val reactionInput =
        """9 ORE => 2 A
          |8 ORE => 3 B
          |7 ORE => 5 C
          |3 A, 4 B => 1 AB
          |5 B, 7 C => 1 BC
          |4 C, 1 A => 1 CA
          |2 AB, 3 BC, 4 CA => 1 FUEL""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.minimumInput(reactions) shouldBe Ingredient(165, "ORE")
    }

    "find fuel for test case three"in {
      val reactionInput =
        """157 ORE => 5 NZVS
          |165 ORE => 6 DCFZ
          |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
          |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
          |179 ORE => 7 PSHF
          |177 ORE => 5 HKGWZ
          |7 DCFZ, 7 PSHF => 2 XJWVT
          |165 ORE => 2 GPVTF
          |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.minimumInput(reactions) shouldBe Ingredient(13312, "ORE")
    }

    "find fuel for test case four"in {
      val reactionInput =
        """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
          |17 NVRVD, 3 JNWZP => 8 VPVL
          |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
          |22 VJHF, 37 MNCFX => 5 FWMGM
          |139 ORE => 4 NVRVD
          |144 ORE => 7 JNWZP
          |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
          |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
          |145 ORE => 6 MNCFX
          |1 NVRVD => 8 CXFTF
          |1 VJHF, 6 MNCFX => 4 RFSQX
          |176 ORE => 6 VJHF""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.minimumInput(reactions) shouldBe Ingredient(180697, "ORE")
    }

    "find fuel for test case five"in {
      val reactionInput =
        """171 ORE => 8 CNZTR
          |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
          |114 ORE => 4 BHXH
          |14 VRPVC => 6 BMBT
          |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
          |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
          |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
          |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
          |5 BMBT => 4 WPTQ
          |189 ORE => 9 KTJDG
          |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
          |12 VRPVC, 27 CNZTR => 2 XDBXC
          |15 KTJDG, 12 BHXH => 5 XCVML
          |3 BHXH, 2 VRPVC => 7 MZWV
          |121 ORE => 7 VRPVC
          |7 XCVML => 6 RJRHP
          |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.minimumInput(reactions) shouldBe Ingredient(2210736, "ORE")
    }
  }

  "Find maximum yield" should {
    "calculate yield for test case one" in {
      val reactionInput =
        """157 ORE => 5 NZVS
          |165 ORE => 6 DCFZ
          |44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL
          |12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ
          |179 ORE => 7 PSHF
          |177 ORE => 5 HKGWZ
          |7 DCFZ, 7 PSHF => 2 XJWVT
          |165 ORE => 2 GPVTF
          |3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)
      val maximumOre = Ingredient(BigInt("1000000000000"), "ORE")

      CriticalPath.calculateYieldForFuel(reactions, maximumOre) shouldBe 82892753
    }

    "calculate yield for test case two" in {
      val reactionInput =
        """2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG
          |17 NVRVD, 3 JNWZP => 8 VPVL
          |53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL
          |22 VJHF, 37 MNCFX => 5 FWMGM
          |139 ORE => 4 NVRVD
          |144 ORE => 7 JNWZP
          |5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC
          |5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV
          |145 ORE => 6 MNCFX
          |1 NVRVD => 8 CXFTF
          |1 VJHF, 6 MNCFX => 4 RFSQX
          |176 ORE => 6 VJHF""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)
      val maximumOre = Ingredient(BigInt("1000000000000"), "ORE")

      CriticalPath.calculateYieldForFuel(reactions, maximumOre) shouldBe 5586022
    }

    "calculate yield for test case three" in {
      val reactionInput =
        """171 ORE => 8 CNZTR
          |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
          |114 ORE => 4 BHXH
          |14 VRPVC => 6 BMBT
          |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
          |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
          |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
          |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
          |5 BMBT => 4 WPTQ
          |189 ORE => 9 KTJDG
          |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
          |12 VRPVC, 27 CNZTR => 2 XDBXC
          |15 KTJDG, 12 BHXH => 5 XCVML
          |3 BHXH, 2 VRPVC => 7 MZWV
          |121 ORE => 7 VRPVC
          |7 XCVML => 6 RJRHP
          |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)
      val maximumOre = Ingredient(BigInt("1000000000000"), "ORE")

      CriticalPath.calculateYieldForFuel(reactions, maximumOre) shouldBe 460664
    }

    "use a recipe to create an ingredient" in  {
      val reactionInput =
        """171 ORE => 8 CNZTR
          |114 ORE => 4 BHXH
          |189 ORE => 9 KTJDG""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)
      val ore = Ingredient(BigInt("475"), "ORE")

      CriticalPath.useRecipe(reactions, Set(ore)) shouldBe Set(Ingredient(BigInt(1), "ORE"), Ingredient(BigInt(8), "CNZTR"),
        Ingredient(BigInt(4), "BHXH"), Ingredient(BigInt(9), "KTJDG"))
    }

    "find the results from the minimum ore per fuel" in  {
      val reactionInput =
        """171 ORE => 8 CNZTR
          |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
          |114 ORE => 4 BHXH
          |14 VRPVC => 6 BMBT
          |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
          |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
          |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
          |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
          |5 BMBT => 4 WPTQ
          |189 ORE => 9 KTJDG
          |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
          |12 VRPVC, 27 CNZTR => 2 XDBXC
          |15 KTJDG, 12 BHXH => 5 XCVML
          |3 BHXH, 2 VRPVC => 7 MZWV
          |121 ORE => 7 VRPVC
          |7 XCVML => 6 RJRHP
          |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      CriticalPath.results(reactions) shouldBe Set(Ingredient(BigInt(1), "FUEL"))
    }

    "run previous reactions when it needs extra ingredients" in  {
      val reactionInput =
        """171 ORE => 8 CNZTR
          |7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL
          |114 ORE => 4 BHXH
          |14 VRPVC => 6 BMBT
          |6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL
          |6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT
          |15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW
          |13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW
          |5 BMBT => 4 WPTQ
          |189 ORE => 9 KTJDG
          |1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP
          |12 VRPVC, 27 CNZTR => 2 XDBXC
          |15 KTJDG, 12 BHXH => 5 XCVML
          |3 BHXH, 2 VRPVC => 7 MZWV
          |121 ORE => 7 VRPVC
          |7 XCVML => 6 RJRHP
          |5 BHXH, 4 VRPVC => 5 LTCX""".stripMargin

      val reactions = CriticalPath.makeReactionListFromRecipe(reactionInput)

      val ingredients = Set(Ingredient(7,"VRPVC"), Ingredient(8,"CNZTR"), Ingredient(4,"BHXH"), Ingredient(9,"KTJDG"), Ingredient(2210141,"ORE"))
      val reaction = Reaction(List(Ingredient(5,"BHXH"), Ingredient(4,"VRPVC")),Ingredient(5,"LTCX"))

      CriticalPath.useRecipe(reaction, reactions, ingredients) shouldBe
        Set(Ingredient(3,"VRPVC"), Ingredient(8,"CNZTR"), Ingredient(3,"BHXH"), Ingredient(9,"KTJDG"), Ingredient(5,"LTCX"), Ingredient(2210027,"ORE"))
    }
  }

}
