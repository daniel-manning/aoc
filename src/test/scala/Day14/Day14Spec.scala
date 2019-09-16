package Day14

import org.scalatest.{Matchers, WordSpec}

class Day14Spec extends WordSpec with Matchers {

  "Generate new recipies" should {
    "create 1 new recipe when there is only 1 digit" in {
      val recipeScoreBoard = "23"
      val elfPosition1 = 1
      val elfPosition2 = 2

      RecipeList.createNewRecipeList(recipeScoreBoard, elfPosition1, elfPosition2) shouldBe "235"
    }
  }

}
