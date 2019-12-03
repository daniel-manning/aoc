package Year2018.Day14

import org.scalatest.{Matchers, WordSpec}

class Day14Spec extends WordSpec with Matchers {

  "Generate new recipies" should {
    "create 1 new recipe when there is only 1 digit" in {
      val recipeScoreBoard = "23"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.createNewRecipeList(recipeScoreBoard, elfPosition1, elfPosition2) shouldBe "235"
    }

    "create 2 new recipes when there are 2 digits" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.createNewRecipeList(recipeScoreBoard, elfPosition1, elfPosition2) shouldBe "3710"
    }
  }

  "Moving Elves" should {
    "move elves by the score of their recipe" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.moveElves(recipeScoreBoard, elfPosition1, elfPosition2) shouldBe (0, 1)

    }
  }

  "NextStep" should {
    "evolve the score board and advance the elves" in {
      val recipeScoreBoard = "3710"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.nextStep((recipeScoreBoard, (elfPosition1, elfPosition2))) shouldBe ("371010", (4, 3))
    }

    "evolve a specific board" in {
      val recipeScoreBoard = "371010124"
      val elfPosition1 = 4
      val elfPosition2 = 8

      RecipeList.nextStep((recipeScoreBoard, (elfPosition1, elfPosition2))) shouldBe ("3710101245", (6, 3))
    }

    "after 9 recipies generate the correct next ten score" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.nextTenRecipes((recipeScoreBoard, (elfPosition1, elfPosition2)), 9) shouldBe "5158916779"
    }

    "after 5 recipies generate the correct next ten score" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.nextTenRecipes((recipeScoreBoard, (elfPosition1, elfPosition2)), 5) shouldBe "0124515891"
    }

    "after 18 recipies generate the correct next ten score" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.nextTenRecipes((recipeScoreBoard, (elfPosition1, elfPosition2)), 18) shouldBe "9251071085"
    }

    "after 2018 recipies generate the correct next ten score" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.nextTenRecipes((recipeScoreBoard, (elfPosition1, elfPosition2)), 2018) shouldBe "5941429882"
    }
  }

  "FirstAppearance" should {
    "find correct match after 9" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.firstAppearance((recipeScoreBoard, (elfPosition1, elfPosition2)), "51589") shouldBe 9
    }

    "find correct match after 5" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.firstAppearance((recipeScoreBoard, (elfPosition1, elfPosition2)), "01245") shouldBe 5
    }

    "find correct match after 18" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.firstAppearance((recipeScoreBoard, (elfPosition1, elfPosition2)), "92510") shouldBe 18
    }

    "find correct match after 2018" in {
      val recipeScoreBoard = "37"
      val elfPosition1 = 0
      val elfPosition2 = 1

      RecipeList.firstAppearance((recipeScoreBoard, (elfPosition1, elfPosition2)), "59414") shouldBe 2018
    }
  }

}
