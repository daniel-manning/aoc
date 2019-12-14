package Year2019

sealed trait Direction
case object LeftTurn extends Direction
case object RightTurn extends Direction

object Direction {
  def apply(turn: Int): Direction = turn match {
    case 0 => LeftTurn
    case 1 => RightTurn
  }
}

case class HullPaintingRobot(hullTiles:Map[Position, Colour], position: Position, intCodeProgramme: IntCodeProgramme){

  def readColour: Colour = hullTiles.getOrElse(position, Black)

  def paintTile(colour: Colour): HullPaintingRobot =
    this.copy(hullTiles = this.hullTiles.updated(position, colour))

  def run = {
    val inputColour = colourToOutput(readColour)
    intCodeProgramme.inputQueue.enqueue(inputColour)
    while(intCodeProgramme.outputQueue.length < 1){
      Thread.sleep(10)
    }

    val newColour = Colour(intCodeProgramme.outputQueue.dequeue().toInt)
    paintTile(newColour)

    while(intCodeProgramme.outputQueue.length < 1){
      Thread.sleep(10)
    }

    val turn = Direction(intCodeProgramme.outputQueue.dequeue().toInt)



  }

  def colourToOutput(colour: Colour): Int = colour match {
    case Black => 0
    case White => 1
  }

}
