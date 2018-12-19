package Day09


case class CircularMarbleList(pointer:Int, marbleList:List[Int], noOfPlayers:Int, currentPlayer:Int, playerScores:List[(Int, Int)]){
  def addNextMarbleBetweenTheNextTwo(): CircularMarbleList = {
    val newMarbleList = (marbleList.slice(0, pointer+1) :+ nextMarble) ++ marbleList.slice(pointer+1, marbleList.length)

    CircularMarbleList( pointer = (pointer + 2) % newMarbleList.length,
                        marbleList = newMarbleList,
                        noOfPlayers,
                        (currentPlayer+1)%noOfPlayers,
                        playerScores )
  }

  def nextMarble:Int = marbleList.max + 1
}

object Day09 extends App {


}


object Marbles {

  def evolve(circularMarbleList: CircularMarbleList):CircularMarbleList = {
    val nextMarble = circularMarbleList.nextMarble
    circularMarbleList.addNextMarbleBetweenTheNextTwo()
  }

  def runTurns(circularMarbleList: CircularMarbleList, turns:Int):CircularMarbleList = {
    (1 to turns).foldRight(circularMarbleList){(a,b) => b.addNextMarbleBetweenTheNextTwo()}
  }

}