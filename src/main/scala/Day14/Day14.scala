package Day14

object Day14 extends App {

  val puzzleInput = 637061
  lazy val next10Score = RecipeList.nextTenRecipes(("37",(0,1)), puzzleInput)
  lazy val firstAppearance = RecipeList.firstAppearance(("37",(0,1)), puzzleInput.toString)


  //println(s"After $puzzleInput recipes the next 10 have value: $next10Score")
  println(s"$puzzleInput appeared after $firstAppearance recipes")
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

  def evolveState(initialState: (String, (Int, Int))): Iterator[(String, (Int, Int))] =
    Iterator.iterate(initialState)(nextStep)

  def nextTenRecipes(initialState: (String, (Int, Int)), afterNoOfRecipes: Int):String =
    evolveState(initialState)
      .dropWhile(l => {println(l._1.length); l._1.length < (afterNoOfRecipes + 10)})
      .next._1
      .slice(afterNoOfRecipes, afterNoOfRecipes + 10)

  def firstAppearance(initialState: (String, (Int, Int)), matchingString: String):Int =
    evolveState(initialState)
      .dropWhile(l => {println(l._1.length); !l._1.contains(matchingString)})
      .next._1
      .indexOf(matchingString)
}
