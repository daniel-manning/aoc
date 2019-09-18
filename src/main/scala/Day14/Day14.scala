package Day14

object Day14 extends App {


}

object RecipeList {
  def nextStep(elfState: (String, (Int, Int))):(String, (Int, Int)) = {
    val (recipeList, (elfP1, elfP2)) = elfState
    val newRecipeList = createNewRecipeList(recipeList, elfP1, elfP2)
    val newElfPos = moveElves(newRecipeList, elfP1, elfP2)

    (newRecipeList, newElfPos)
  }


  def moveElves(recipeScoreBoard: String, elfPosition1: Int, elfPosition2: Int): (Int, Int) = {
    val positionShift1 = lookUpScore(recipeScoreBoard, elfPosition1) + 1
    val positionShift2 = lookUpScore(recipeScoreBoard, elfPosition2) + 1

    ((elfPosition1 + positionShift1) % recipeScoreBoard.length, (elfPosition2 + positionShift2) % recipeScoreBoard.length)
  }

  def createNewRecipeList(recipeList:String, elfPosition1:Int, elfPosition2:Int): String = {

    val score1 = lookUpScore(recipeList, elfPosition1)
    val score2 = lookUpScore(recipeList, elfPosition2)

    recipeList ++ (score1 + score2).toString
  }

  private def lookUpScore(recipeScoreBoard: String, position: Int):Int =
    recipeScoreBoard.substring(position, position+1).toInt
}
