package Day09

import scala.collection.mutable

object Day09 extends App {
Marbles.addToList()
}


object Marbles {

  def addToList():Unit = {

    val list = mutable.LinkedList(0)
    list.insert(mutable.LinkedList(1))


  }



}