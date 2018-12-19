package Day08

import scala.io.Source

case class Node(childNodes:List[Node], data:List[Int])



object Day08 extends App {

  val headerData:List[Int] = Source.fromResource("day08_input").getLines.toList.head.split(" ").map(_.toInt).toList
  val headerStructure = Marshaller.marshall(headerData)._1
  val sumTotal = Marshaller.sumOverNodes(headerStructure)
  println(s"sumTotal over structure is: $sumTotal")

}

object Day08_02 extends App {

  val headerData:List[Int] = Source.fromResource("day08_input").getLines.toList.head.split(" ").map(_.toInt).toList
  val headerStructure:Node = Marshaller.marshall(headerData)._1
  val valueOfRootNode = Marshaller.valueOfNode(headerStructure)
  println(s"Value of root node of structure is: $valueOfRootNode")

}


object Marshaller {

  def marshall(data: List[Int]):(Node, List[Int]) = {
    //println(s"data: $data")

    val noOfChildNodes = data.head
    val noOfDataNodes = data.tail.head

    if(noOfChildNodes == 0) {
      (Node(List(), data.slice(2, noOfDataNodes + 2)), data.drop(noOfDataNodes + 2))
    }else{
      //println("hello")
      //println(s"noOfChildNodes: $noOfChildNodes")
      val children = (1 to noOfChildNodes).foldRight((List[Node](), data.drop(2))){
        (_, b) =>
          //println(s"going to marshall - { ${b._2}")
        val (children, dataLeftOver) = marshall(b._2)
          //println(s"after marshall: ${(children, dataLeftOver)}")
          (b._1 :+ children, dataLeftOver)
      }

      (Node(children._1, children._2.take(noOfDataNodes)), children._2.drop(noOfDataNodes))
    }
  }

  def sumOverNodes(node:Node):Int = {
    node.data.sum + node.childNodes.map(sumOverNodes).sum
  }

  def valueOfNode(node:Node):Int = {
    if(node.childNodes.isEmpty){
      node.data.sum
    }else{
      val referenceNodes = node.childNodes.zipWithIndex
      val references = node.data
        references.map(r => referenceNodes.find(p => p._2 == (r-1)).map(a => valueOfNode(a._1)).getOrElse(0)).sum
    }
  }


}