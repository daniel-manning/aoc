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

      Day07.lookupNextTask(dependencyGraph, completedTasks.toList, List(), everythingLeftToComplete) shouldBe ("A", List("F"))
    }

    "project plan with 2 workers" in {
      val workList = List(WorkListItem(1, "C", (0,3)),
                          WorkListItem(1, "A", (3,4)),
                          WorkListItem(2, "F", (3,9)),
                          WorkListItem(1, "B", (4,6)),
                          WorkListItem(1, "D", (6,10)),
                          WorkListItem(1, "E", (10,15)))

      Day07.timeToComplete(workList) shouldBe 15
    }

    "distribute work correctly" ignore  {
      val workList = List(WorkListItem(1, "C", (0,63)),
        WorkListItem(1, "A", (63,124)),
        WorkListItem(2, "F", (63,129)),
        WorkListItem(1, "B", (124,186)),
        WorkListItem(1, "D", (186,250)),
        WorkListItem(1, "E", (250,315)))

      val dependencyGraph = Day07.generateDependencyGraph(stepData)

      Day07.generateWorkList(dependencyGraph, 2) shouldBe workList
    }

    "calculate duration" in {
      Day07.calculateDuration(0, "C") shouldBe (0, 63)
    }
  }


}
