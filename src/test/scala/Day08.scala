import Day08.{Marshaller, Node}
import org.scalatest.{Matchers, WordSpec}

class Day08 extends WordSpec with Matchers {

  "Marshaller" should {
    "marshall a childless node" in {
      val data = List(0, 3, 2, 2, 2)
      val result = Node(List(), List(2,2,2))

      Marshaller.marshall(data)._1 shouldBe result
    }

    "marshall a node with one child node" in {
      val data = List(1, 3, 0, 2, 5, 4, 2, 2, 2)
      val result = Node(List(Node(List(), List(5,4))), List(2,2,2))

      Marshaller.marshall(data)._1 shouldBe result
    }

    "marshall more complicated structure" in {
      val data = List(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)

      val nodeD = Node(List(), List(99))
      val nodeC = Node(List(nodeD), List(2))
      val nodeB = Node(List(), List(10, 11, 12))
      val nodeA = Node(List(nodeB, nodeC), List(1, 1, 2))

      Marshaller.marshall(data)._1 shouldBe nodeA

    }

    "calculate sum over node structure" in {
      val data = List(2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2)
      val nodes = Marshaller.marshall(data)._1

      Marshaller.sumOverNodes(nodes) shouldBe 138
    }
  }


}
