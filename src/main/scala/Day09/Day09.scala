package Day09


case class CircularMarbleList(pointer:Int, marbleList:List[Int], noOfPlayers:Int, currentPlayer:Int, marbleToPlace:Int, playerScores:List[(Int, Int)]){
  def keepMarbleTakeOneSevenBack(): CircularMarbleList = {
    val newMarbleList = marbleList.slice(0, pointer-8) ++ marbleList.slice(pointer-7, marbleList.length)
    println(s"takenMarbleList: ${marbleList.slice(pointer-8,pointer-7)} for pointer: ${pointer}")
    val takenMarble:Int = marbleList.slice(pointer-8,pointer-7).head
    val playerAffectedScore:Int = playerScores.find(p => p._1 == currentPlayer).getOrElse((currentPlayer,0))._2
    val newPlayerScores = playerScores.filter(p => p._1 == currentPlayer) :+ (currentPlayer, playerAffectedScore + takenMarble + nextMarble)

    CircularMarbleList((pointer - 7) % newMarbleList.length,
                       newMarbleList,
                       noOfPlayers,
                       (currentPlayer+1)%noOfPlayers,
                       marbleToPlace + 1,
                       newPlayerScores)
  }

  def addNextMarbleBetweenTheNextTwo(): CircularMarbleList = {
    val newMarbleList = (marbleList.slice(0, pointer+1) :+ nextMarble) ++ marbleList.slice(pointer+1, marbleList.length)

    CircularMarbleList( pointer = (pointer + 2) % newMarbleList.length,
                        marbleList = newMarbleList,
                        noOfPlayers,
                        (currentPlayer+1)%noOfPlayers,
                        marbleToPlace + 1,
                        playerScores )
  }

  def nextMarble:Int = marbleToPlace
}

object Day09 extends App {


}


object Marbles {

  def evolve(circularMarbleList: CircularMarbleList):CircularMarbleList = {
    val nextMarble = circularMarbleList.nextMarble
    if(nextMarble % 23 == 0) {
      circularMarbleList.keepMarbleTakeOneSevenBack()
    }else{
      circularMarbleList.addNextMarbleBetweenTheNextTwo()
    }
  }

  def runTurns(circularMarbleList: CircularMarbleList, turns:Int):CircularMarbleList = {
    (1 to turns).foldRight(circularMarbleList){(a,b) => Marbles.evolve(b)}
  }

}