package Year2018

case class Position(x: Int, y: Int)

sealed trait Direction
case object North extends Direction
case object South extends Direction
case object East extends Direction
case object West extends Direction

sealed trait Orientation
case object EastWest extends Orientation
case object NorthSouth extends Orientation

sealed trait TrackPiece
case class Straight(orientation: Orientation, position: Position) extends TrackPiece
case class Curve(position: Position) extends TrackPiece
case class Intersection(position: Position) extends TrackPiece
case object Empty extends TrackPiece

case class Cart(direction: Direction, position: Position) extends TrackPiece


case class Track(tracks: List[TrackPiece]){

  //def moveCarts():Track =


}
