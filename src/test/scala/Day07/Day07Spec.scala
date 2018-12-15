package Day07

import org.scalatest.{Matchers, WordSpec}

import scala.collection.immutable.TreeSet

class Day07Spec extends WordSpec with Matchers{
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
      Day07.marshallData(List(stepData)) shouldBe List(("C", "A"))
    }


    "generate graph" in {
      val edges = Graph(List(("C","A"), ("C","F"), ("A","B"), ("A","D"), ("B","E"), ("D","E"), ("F","E")))
      val dependencyGraph = Day07.generateDependencyGraph(stepData)
      dependencyGraph shouldBe edges
    }

    "generate node list" in {
      val dependencyGraph = Day07.generateDependencyGraph(stepData)

      dependencyGraph.nodes() shouldBe TreeSet("A", "B", "C", "D", "E", "F")
    }

    "generate linear repr" in {
      val dependencyGraph = Day07.generateDependencyGraph(stepData)
      Day07.defineLinearTaskRepr(dependencyGraph) shouldBe List("C", "A", "B", "D", "F", "E")
    }

    "work out next task" in {
      val dependencyGraph = Day07.generateDependencyGraph(stepData)
      val toSet:Set[Node] = dependencyGraph.edges.foldRight(Set[Node]()){(a,b) => b + a._2 }
      val completedTasks = Set("C")
      val everythingLeftToComplete = toSet.diff(completedTasks)

      Day07.lookupNextTask(dependencyGraph, completedTasks.toList, everythingLeftToComplete) shouldBe "A"
    }
  }


}
