package Day12

import scala.io.Source

case class EvolutionRules(ruleset: Map[String, Char])

object Day12 extends App {

  val everything = Source.fromResource("day12_input").getLines.toList

  val initialState = everything.head.drop(15)

  val ruleSetPattern = "([.#]+) => ([.#])".r
  val ruleSetPairs:List[(String, Char)] = everything.slice(3, everything.length).map {
    l => ruleSetPattern.findFirstMatchIn(l).map(mat => (mat.group(1), mat.group(2).head)).get
  }

  val rulesSet = ruleSetPairs.toMap

  val potPositions = CellularAutomata.countPotPositions(20, rulesSet, initialState)

  println(s"The sum of the positions of the evolved state: $potPositions")
}

object CellularAutomata {
  type IndexedState = List[(Char,Int)]

  def countPotPositions(i: Int, ruleSet: Map[String, Char], initalState: String):Int = {
    val states = CellularAutomata.evolve(20, ruleSet, initalState)
    val state = states.last

    state.filter(_._1 == '#').map(_._2).sum
  }

  def evolve(noOfTurns: Int, ruleSet: Map[String, Char], initalState: String):List[IndexedState] = {
    (1 to noOfTurns).foldRight(List[IndexedState](indexTheInitialState(initalState))){
      (_, b) => b :+ nextGeneration(ruleSet, b.last)
    }
  }

  def nextGeneration(ruleSet: Map[String, Char], state: IndexedState):IndexedState = {
    val headIndex = state.head._2
    val pre2 = (ruleSet.getOrElse("...." ++ state.take(1).map(_._1), '.'), headIndex - 2)
    val pre1 = (ruleSet.getOrElse("..." ++ state.take(2).map(_._1), '.'), headIndex - 1)
    val first = (ruleSet.getOrElse(".." ++ state.take(3).map(_._1), '.'), headIndex)
    val second = (ruleSet.getOrElse("." ++ state.take(4).map(_._1), '.'), headIndex + 1)

    val mainSection:IndexedState = state.sliding(5).map( pattern => (ruleSet.getOrElse(pattern.map(_._1).mkString, '.'), pattern.drop(2).head._2)).toList

    val lastIndex = state.last._2
    val secondLast  = (ruleSet.getOrElse( state.takeRight(4).map(_._1).mkString ++ ".", '.'), lastIndex - 1)
    val last = (ruleSet.getOrElse(state.takeRight(3).map(_._1).mkString ++ "..", '.'), lastIndex)
    val post1 = (ruleSet.getOrElse(state.takeRight(2).map(_._1).mkString ++ "...", '.'), lastIndex + 1)
    val post2 = (ruleSet.getOrElse(state.takeRight(1).map(_._1).mkString ++ "....", '.'), lastIndex + 2)

    (pre2 :: pre1 :: first :: second :: List()).dropWhile(_._1 == '.') ++ mainSection ++ (secondLast :: last :: post1 :: post2 :: List()).reverse.dropWhile(_._1 == '.').reverse
  }

  def flattenToString(input:List[(Char, Int)]):String = input.map(_._1).mkString

  def indexTheInitialState(state:String):IndexedState = state.zipWithIndex.toList

}
