package Year2018

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.immutable.TreeSet

class Day07Spec extends AnyWordSpec with Matchers {
  val stepData = List("Step C must be finished before step A can begin.",
    "Step C must be finished before step F can begin.",
    "Step A must be finished before step B can begin.",
    "Step A must be finished before step D can begin.",
    "Step B must be finished before step E can begin.",
    "Step D must be finished before step E can begin.",
    "Step F must be finished before step E can begin.")

  "Day07" should {
    "marshall data" in {
      val stepData = "Step C must be finished before step A can begin."
      Day07.marshallData(List(stepData)) shouldBe List(Edge(WorkNode("C"), WorkNode("A")))
    }


    "generate graph" in {
      val edges = Graph(List(
        Edge(WorkNode("C"),WorkNode("A")),
        Edge(WorkNode("C"),WorkNode("F")),
        Edge(WorkNode("A"),WorkNode("B")),
        Edge(WorkNode("A"),WorkNode("D")),
        Edge(WorkNode("B"),WorkNode("E")),
        Edge(WorkNode("D"),WorkNode("E")),
        Edge(WorkNode("F"),WorkNode("E"))))
      val dependencyGraph = Day07.generateDependencyGraph(stepData)
      dependencyGraph shouldBe edges
    }

    "generate WorkNode list" in {
      val dependencyGraph = Day07.generateDependencyGraph(stepData)

      dependencyGraph.nodes() shouldBe TreeSet(WorkNode("A"), WorkNode("B"), WorkNode("C"), WorkNode("D"), WorkNode("E"), WorkNode("F"))
    }

    "generate linear repr" in {
      val dependencyGraph = Day07.generateDependencyGraph(stepData)
      Day07.defineLinearTaskRepr(dependencyGraph) shouldBe List(WorkNode("C"), WorkNode("A"), WorkNode("B"), WorkNode("D"), WorkNode("F"), WorkNode("E"))
    }

    "work out next task" in {
      val dependencyGraph = Day07.generateDependencyGraph(stepData)
      val toSet:Set[WorkNode] = dependencyGraph.edges.foldRight(Set[WorkNode]()){(a,b) => b + a.node2 }
      val completedTasks = Set(WorkNode("C"))
      val everythingLeftToComplete = toSet.diff(completedTasks)

      Day07.lookupNextTask(dependencyGraph, completedTasks.toList, List(), everythingLeftToComplete) shouldBe (WorkNode("A"), List(WorkNode("F")))
    }

    "project plan with 2 workers" in {
      val workList = List(WorkListItem(1, WorkNode("C"), (0,3)),
                          WorkListItem(1, WorkNode("A"), (3,4)),
                          WorkListItem(2, WorkNode("F"), (3,9)),
                          WorkListItem(1, WorkNode("B"), (4,6)),
                          WorkListItem(1, WorkNode("D"), (6,10)),
                          WorkListItem(1, WorkNode("E"), (10,15)))

      Day07.timeToComplete(workList) shouldBe 15
    }

    "distribute work correctly" ignore  {
      val workList = List(WorkListItem(1, WorkNode("C"), (0,63)),
        WorkListItem(1, WorkNode("A"), (63,124)),
        WorkListItem(2, WorkNode("F"), (63,129)),
        WorkListItem(1, WorkNode("B"), (124,186)),
        WorkListItem(1, WorkNode("D"), (186,250)),
        WorkListItem(1, WorkNode("E"), (250,315)))

      val dependencyGraph = Day07.generateDependencyGraph(stepData)

      Day07.generateWorkList(dependencyGraph, 2) shouldBe workList
    }

    "calculate duration" in {
      Day07.calculateDuration(0, WorkNode("C")) shouldBe (0, 63)
    }
  }


}
