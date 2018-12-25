package Day12

case class EvolutionRules(ruleset: Map[String, Char])

object Day12 {




}

object CellularAutomata {
  def countPotPositions(i: Int, ruleSet: Map[String, Char], initalState: String):Int = {
    ???
  }

  def evolve(noOfTurns: Int, ruleSet: Map[String, Char], initalState: String):List[String] = {
    (1 to noOfTurns).foldRight(List[String](initalState)){
      (_, b) => b :+ nextGeneration(ruleSet, b.last)
    }
  }

  def nextGeneration(ruleSet: Map[String, Char], state: String):String = {
    val pre2 = ruleSet.getOrElse("...." ++ state.take(1), '.')
    val pre1 = ruleSet.getOrElse("..." ++ state.take(2), '.')
    val first = ruleSet.getOrElse(".." ++ state.take(3), '.')
    val second = ruleSet.getOrElse("." ++ state.take(4), '.')

    val mainSection = state.sliding(5).map( pattern => ruleSet.getOrElse(pattern, '.')).toList.mkString

    val secondLast  = ruleSet.getOrElse( state.takeRight(4) ++ ".", '.')
    val last = ruleSet.getOrElse(state.takeRight(3) ++ "..", '.')
    val post1 = ruleSet.getOrElse(state.takeRight(2) ++ "...", '.')
    val post2 = ruleSet.getOrElse(state.takeRight(1) ++ "....", '.')

    (pre2 :: pre1 :: first :: second :: List()).dropWhile(_ == '.').mkString ++ mainSection ++ (secondLast :: last :: post1 :: post2 :: List()).reverse.dropWhile(_ == '.').reverse.mkString
  }

}
