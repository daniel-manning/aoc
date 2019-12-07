package Year2018

case class CircularMarbleList(pointer:Int, marbleList:List[Int], noOfPlayers:Int, currentPlayer:Int, marbleToPlace:Int, playerScores:List[(Int, Int)]){
  def keepMarbleTakeOneSevenBack(): CircularMarbleList = {
    val newMarbleList = marbleList.slice(0, circle(pointer-8)) ++ marbleList.slice(circle(pointer-7), marbleList.length)
    //println(s"takenMarbleList: ${marbleList.slice(circle(pointer-8),circle(pointer-7))} for pointer: ${pointer}")
    val takenMarble:Int = marbleList.slice(circle(pointer-8),circle(pointer-7)).head
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

  def circle(pos:Int):Int = if(pos.sign == -1) marbleList.length + pos else pos

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

  def runGame(noOfPlayers:Int, maximum:Int):Int = {
    val initialState = CircularMarbleList(pointer = 0, marbleList = List(0,1), noOfPlayers, currentPlayer = 2, marbleToPlace = 2, playerScores = List())
    val finalState = iterateUntilFinished(initialState, maximum)

    finalState.playerScores.maxBy(_._2)._2
  }

  def iterateUntilFinished(circularMarbleList:CircularMarbleList, limit:Int):CircularMarbleList = {
    if(circularMarbleList.marbleToPlace == limit + 1){
      circularMarbleList
    }else{
      iterateUntilFinished(Marbles.evolve(circularMarbleList), limit)
    }
  }

}