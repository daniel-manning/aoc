package Day09


case class CircularMarbleList(pointer:Int, marbleList:List[Int]){
  def moveTwoOnAndAddNextMarble(): CircularMarbleList ={
    ???
  }
}

object Day09 extends App {
Marbles.addToList()
}


object Marbles {

  def addToList():Unit = {

    val list = mutable.LinkedList(0)
    list.insert(mutable.LinkedList(1))


  }



}