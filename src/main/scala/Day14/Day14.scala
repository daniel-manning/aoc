package Day14

object Day14 extends App {


}

object RecipeList {
  def createNewRecipeList(recipeList:String, elfPosition1:Int, elfPosition2:Int): String = {

    val score1 = recipeList.substring(elfPosition1, elfPosition1 + 1).toInt
    val score2 = recipeList.substring(elfPosition2, elfPosition2 + 1).toInt


    recipeList ++ (score1 + score2).toString
  }
}
