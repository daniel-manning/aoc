package Year2018.Day12

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
  //val potPositions = CellularAutomata.countPotPositions(50000000000l, rulesSet, initialState)

  println(s"The sum of the positions of the evolved state: $potPositions")
}

object CellularAutomata {
  type IndexedState = List[Int]

  def countPotPositions(noOfTurns: Long, ruleSet: Map[String, Char], initalState: String):Int = {
    val state = CellularAutomata.evolve(noOfTurns, ruleSet, initalState)

    state.sum
  }

  def indexTheInitialState(initalState: String):List[Int] = {
    println(s"state: ${initalState.zipWithIndex}")
    initalState.zipWithIndex.filter(p => p._1 == '#').toList.map(_._2)
  }

  def evolve(noOfTurns: Long, ruleSet: Map[String, Char], initalState: String):IndexedState = {
    //Sigh
    var i = 0L
    var b = indexTheInitialState(initalState)
    while(i < noOfTurns) {
      b = nextGeneration(ruleSet, b)
      i+=1
      if(i % 10000L == 0) println(s"----------$i---------")
    }

    b
    /*(1l to noOfTurns).foldRight(indexTheInitialState(initalState)){
      (_, b) => nextGeneration(ruleSet, b)
    }*/
  }

  def nextGeneration(ruleSet: Map[String, Char], state: IndexedState):IndexedState = {
/*    val headIndex = state.head._2
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

    (pre2 :: pre1 :: first :: second :: List()).dropWhile(_._1 == '.') ++ mainSection ++ (secondLast :: last :: post1 :: post2 :: List()).reverse.dropWhile(_._1 == '.').reverse*/

    //indexed state is a list of all the plants
    //loop through plants and generate all five positions it could be in to create set of new plants
    //there will be duplicates but much less than the alternative
    val newGeneration:Set[Int] =state.foldRight(Set[Int]()){
      (a, b) => generateAllPossibleWindowsAndEvolve(a, state, ruleSet).foldRight(b){
        (c,d) => if(c._1 == '#') d + c._2 else d
      }
    }

    newGeneration.toList.sorted
  }

  def generateAllPossibleWindowsAndEvolve(potPosition:Int, state:IndexedState, ruleSet: Map[String, Char]):List[(Char, Int)] = {
    List((List(potPosition, potPosition + 1, potPosition + 2, potPosition + 3, potPosition + 4), potPosition + 2),
      (List(potPosition - 1, potPosition, potPosition + 1, potPosition + 2, potPosition + 3), potPosition + 1),
      (List(potPosition - 2, potPosition - 1, potPosition, potPosition + 1, potPosition + 2), potPosition),
      (List(potPosition - 3, potPosition - 2, potPosition - 1, potPosition, potPosition + 1), potPosition - 1),
      (List(potPosition - 4, potPosition - 3, potPosition - 2, potPosition - 1, potPosition), potPosition - 2),
    ).map(a => (a._1.map(n => if(state.contains(n)) '#' else '.').mkString, a._2))
     .map(b => (ruleSet.getOrElse(b._1, '.'), b._2))
  }

}
