package Year2018

import scala.io.Source

object Day02 extends App {

  val listOfIds = Source.fromResource("2018/day02_input").getLines.toList
  val checksum = Checksum.checksum(listOfIds)
  println(s"Checksum of box ids: $checksum")

  val boxes = Checksum.boxesWithEditedName(Checksum.whichBoxesDifferByOneEdit(listOfIds))
  println(s"Boxes which differ by one edit: $boxes")
}



object Checksum {

  def doublesAndTriples(input:String):(Int,Int) = {
    val groupedLetters = input.toSeq.groupBy(identity).view.mapValues(_.unwrap).map(_._2.length)
    val exactlyTwoLetters = groupedLetters.count(_ == 2)
    val exactlyThreeLetters = groupedLetters.count(_ == 3)

    (if(exactlyTwoLetters>=1){ 1 }else{ 0 }, if(exactlyThreeLetters>=1){ 1 }else{ 0 })
  }

  def checksum(input:Seq[String]):Int = {
    val totals = input.map(doublesAndTriples).foldRight((0,0)){ (a,b) => (a._1 + b._1, a._2 + b._2)}

    totals._1 * totals._2
  }

  def hammingDistance(input1:String, input2:String):List[Int] = {
    input1.zipWithIndex.zip(input2).filter(p => p._1._1 != p._2).toList.map(_._1._2)
  }

  def whichBoxesDifferByOneEdit(listOfBoxIds:Seq[String]):Seq[(String, String)] = {
    def compareBoxes(headBoxId:String, tailBoxes:Seq[String]):Seq[(String, String)] = {
      if(tailBoxes.isEmpty){
        Seq()
      }else {
        val correctBoxes = tailBoxes.filter(id => hammingDistance(headBoxId, id).length == 1).map((headBoxId, _))
        correctBoxes ++ compareBoxes(tailBoxes.head, tailBoxes.tail)
      }
    }

    compareBoxes(listOfBoxIds.head, listOfBoxIds.tail)
  }

  def boxesWithEditedName(boxes:Seq[(String, String)]):Seq[String] = {
    boxes.map { boxIDs =>
      val editPosition = hammingDistance(boxIDs._1, boxIDs._2).head
      s"${boxIDs._1.substring(0, editPosition)}${boxIDs._1.substring(editPosition+1)}"
    }
  }

}