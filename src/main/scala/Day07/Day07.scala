package Day07

import scala.collection.immutable.SortedSet
import scala.io.Source

//nodes:List[Node],
case class Graph(edges:List[Edge]){

  def nodes():Set[Node] = {
    edges.foldRight(SortedSet[Node]()(new Ordering[Node]() {
      override def compare(x: Node, y: Node): Int =
        Ordering[String].compare(x, y)
    })){(a,b) => b + a._1 + a._2}
  }

}

object Day07Runner extends App {
  val taskOrders:List[String] = Source.fromResource("day07_input").getLines.toList
  val taskOrder = Day07.defineLinearTaskRepr(Day07.generateDependencyGraph(taskOrders))
  println(s"We need to follow the tasks in this order: ${taskOrder.mkString}")
}


object Day07 {
  val stepDependencyPattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r


  def marshallData(dependencies:List[String]):List[Edge] = {
    dependencies.map(l => stepDependencyPattern.findFirstMatchIn(l).map( mat => (mat.group(1), mat.group(2))).get)
  }

  def generateDependencyGraph(stepData:List[String]):Graph = {
    Graph(marshallData(stepData))
  }


  def defineLinearTaskRepr(dependencyGraph:Graph):List[Node] = {
    val toSet:Set[Node] = dependencyGraph.edges.foldRight(Set[Node]()){(a,b) => b + a._2 }
    val fromSet:Set[Node] = dependencyGraph.edges.foldRight(Set[Node]()){(a,b) => b + a._1}

    println(s"toSet: $toSet")
    println(s"fromSet: $fromSet")
    println(s"nodes: ${dependencyGraph.nodes()}")

    val source = dependencyGraph.nodes().diff(toSet)
    println(s"source: $source")

    val everythingLeftToComplete = toSet.diff(source)

    iterateUntilDone(dependencyGraph, List(source.min), source.diff(Set(source.min)).toList, everythingLeftToComplete)
  }

  def iterateUntilDone(dependencyGraph:Graph, completedTasks:List[Node], workAvailableToPlay:List[Node], everythingLeftToComplete:Set[Node]):List[Node] = {
    if(everythingLeftToComplete.isEmpty){
      completedTasks
    }else{
        val (justCompleted, workLeftAvailableToPlay) = lookupNextTask(dependencyGraph, completedTasks, workAvailableToPlay, everythingLeftToComplete)
        println(s"justCompleted: ${justCompleted} workLeftAvailableToPlay: $workLeftAvailableToPlay")
        iterateUntilDone(dependencyGraph, completedTasks :+ justCompleted, workLeftAvailableToPlay, everythingLeftToComplete.filterNot(_ == justCompleted))
    }
  }

  def lookupNextTask(dependencyGraph:Graph, completedTasks:List[Node], workAvailableToPlay:List[Node], everythingLeftToComplete:Set[Node]):(Node, List[Node])  = {
    //println(s"extras: $extras")
    val waitingToTickOff = dependencyGraph.edges.groupBy(_._2)
      .filter(a => everythingLeftToComplete.contains(a._1))
      .filter(a => a._2.forall(b => completedTasks.contains(b._1)))
      .toList.map(_._1)

    val next = (workAvailableToPlay ++ waitingToTickOff).min
    (next, (workAvailableToPlay ++ waitingToTickOff).distinct.filterNot(_ == next))
  }

  def timeToComplete(dependencyGraph: Graph, noOfWorkers: Int):Int = {
    ???
  }

}


