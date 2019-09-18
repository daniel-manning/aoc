package Day14

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
  }

}
