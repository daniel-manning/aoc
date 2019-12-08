package Year2018

import scala.collection.immutable.SortedSet
import scala.io.Source

case class WorkNode(value: String) extends Ordered[WorkNode]{

  def compare(that: WorkNode): Int = this.value compare that.value

  override def toString: String = value
}

case class Edge(node1: WorkNode, node2: WorkNode)

//nodes:List[Node],
case class Graph(edges:List[Edge]){

  def nodes():Set[WorkNode] = {
    edges.foldRight(SortedSet[WorkNode]()(new Ordering[WorkNode]() {
      override def compare(x: WorkNode, y: WorkNode): Int =
        Ordering[String].compare(x.value, y.value)
    })){(a,b) => b + a.node1 + a.node2}
  }

}

case class WorkListItem(worker:Int, task:WorkNode, fromTil:(Int, Int))

object Day07Runner extends App {
  val taskOrders:List[String] = Source.fromResource("2018/day07_input").getLines.toList
  val taskOrder = Day07.defineLinearTaskRepr(Day07.generateDependencyGraph(taskOrders))
  println(s"We need to follow the tasks in this order: ${taskOrder.mkString}")
}

object Day07Runner_02 extends App {
  val taskOrders:List[String] = Source.fromResource("2018/day07_input").getLines.toList
  val timeToBuild = Day07.timeToComplete(Day07.generateWorkList(Day07.generateDependencyGraph(taskOrders), noOfWorkers = 5))
  println(s"We need this amount of time to finish the build: $timeToBuild")
}

object Day07 {
  val stepDependencyPattern = "Step ([A-Z]) must be finished before step ([A-Z]) can begin.".r


  def marshallData(dependencies:List[String]):List[Edge] = {
    dependencies.map(l => stepDependencyPattern.findFirstMatchIn(l).map( mat => Edge(WorkNode(mat.group(1)), WorkNode(mat.group(2)))).get)
  }

  def generateDependencyGraph(stepData:List[String]):Graph = {
    Graph(marshallData(stepData))
  }


  def defineLinearTaskRepr(dependencyGraph:Graph):List[WorkNode] = {
    val toSet:Set[WorkNode] = dependencyGraph.edges.foldRight(Set[WorkNode]()){(a,b) => b + a.node2 }

    val source = dependencyGraph.nodes().diff(toSet)

    val everythingLeftToComplete = toSet.diff(source)

    iterateUntilDone(dependencyGraph, List(source.min), source.diff(Set(source.min)).toList, everythingLeftToComplete)
  }

  def iterateUntilDone(dependencyGraph:Graph, completedTasks:List[WorkNode], workAvailableToPlay:List[WorkNode], everythingLeftToComplete:Set[WorkNode]): List[WorkNode] = {
    if(everythingLeftToComplete.isEmpty){
      completedTasks
    }else{
        val (justCompleted, workLeftAvailableToPlay) = lookupNextTask(dependencyGraph, completedTasks, workAvailableToPlay, everythingLeftToComplete)
        iterateUntilDone(dependencyGraph, completedTasks :+ justCompleted, workLeftAvailableToPlay, everythingLeftToComplete.filterNot(_ == justCompleted))
    }
  }

  def lookupNextTask(dependencyGraph:Graph, completedTasks:List[WorkNode], workAvailableToPlay:List[WorkNode], everythingLeftToComplete:Set[WorkNode]):(WorkNode, List[WorkNode])  = {
    val waitingToTickOff = dependencyGraph.edges.groupBy(_.node2)
      .filter(a => everythingLeftToComplete.contains(a._1))
      .filter(a => a._2.forall(b => completedTasks.contains(b.node1)))
      .toList.map(_._1)

    val next = (workAvailableToPlay ++ waitingToTickOff).min
    (next, (workAvailableToPlay ++ waitingToTickOff).distinct.filterNot(_ == next))
  }

  //-----------------------------
  def timeToComplete(workList:List[WorkListItem]):Int = {
    workList.maxBy(_.fromTil._2).fromTil._2
  }

  def generateWorkList(dependencyGraph: Graph, noOfWorkers: Int):List[WorkListItem] = {
    val toSet:Set[WorkNode] = dependencyGraph.edges.foldRight(Set[WorkNode]()){(a,b) => b + a.node2 }

    val source = dependencyGraph.nodes().diff(toSet)
    val everythingLeftToComplete = toSet.diff(source)

    iterateWorkListUntilDone(dependencyGraph, noOfWorkers, List(), List(), source.toList, everythingLeftToComplete, 0)
  }

  def iterateWorkListUntilDone(dependencyGraph:Graph,
                               noOfWorkers: Int,
                               completedTasks:List[WorkListItem],
                               currentWork:   List[WorkListItem],
                               workAvailableToPlay:List[WorkNode],
                               everythingLeftToComplete:Set[WorkNode],
                               currentTime:Int):List[WorkListItem] = {
    if(everythingLeftToComplete.isEmpty && workAvailableToPlay.isEmpty && currentWork.isEmpty){
      completedTasks
    }else{
      //fill up workers from available work
      val occupiedWorkers = currentWork.map(_.worker)
      val availableWorkers = (1 to noOfWorkers).toSet.diff(occupiedWorkers.toSet)
      val newWorkListItems = workAvailableToPlay zip availableWorkers map {a => WorkListItem(a._2, a._1, calculateDuration(currentTime, a._1)) }

      val workNotActioned = workAvailableToPlay.toSet.diff(newWorkListItems.map(_.task).toSet).toList

      val allOngoingWork = currentWork ++ newWorkListItems
      //move to next time a task finish (may be multiple tasks finishing at the same time
      val endTime:Int = allOngoingWork.minBy(_.fromTil._2).fromTil._2
      val workNowEnded:List[WorkListItem] = allOngoingWork.filter(_.fromTil._2 == endTime)
      val leftoverOngoingWork = allOngoingWork.toSet.diff(workNowEnded.toSet).toList

      //fill up available work
      val waitingToTickOff:List[WorkNode] = dependencyGraph.edges.groupBy(_.node2)
        .filter(a => everythingLeftToComplete.contains(a._1))
        .filter(a => a._2.forall(b => (completedTasks ++ workNowEnded).map(_.task).contains(b.node1)))
        .toList.map(_._1)

      //iterate
      iterateWorkListUntilDone(dependencyGraph, noOfWorkers,
        completedTasks ++ workNowEnded,
        leftoverOngoingWork,
        (workNotActioned ++ waitingToTickOff).distinct,
        everythingLeftToComplete.diff(waitingToTickOff.toSet),
        endTime
      )
    }
  }

  def calculateDuration(currentTime: Int, node: WorkNode):(Int, Int) = {
    val duration = node.value.head.toInt - 4
    (currentTime, currentTime + duration)
  }

}


