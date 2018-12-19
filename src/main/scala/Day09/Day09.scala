package Day09


case class CircularMarbleList(pointer:Int, marbleList:List[Int]){
  def addNextMarbleBetweenTheNextTwo(): CircularMarbleList ={
    val nextMarble = marbleList.max + 1
    val newMarbleList = (marbleList.slice(0, pointer+1) :+ nextMarble) ++ marbleList.slice(pointer+1, marbleList.length)
    CircularMarbleList((pointer + 2) % marbleList.length, newMarbleList)
  }
}

object Day09 extends App {


}


object Marbles {




}