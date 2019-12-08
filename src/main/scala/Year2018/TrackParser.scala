package Year2018

object TrackParser {

  def parse(trackString: String): (Track, List[Cart]) = {
    val trackPieces = trackString.split("\n").zipWithIndex.toList.flatMap {
      l => l._1.zipWithIndex.map {
        m => m._1 match {
          case '/' => Curve(Position(m._2, l._2))
          case '\\' => Curve(Position(m._2, l._2))
          case '-' => Straight(EastWest, Position(m._2, l._2))
          case '|' => Straight(NorthSouth, Position(m._2, l._2))
          case '+' => Intersection(Position(m._2, l._2))
            //carts
          case '^' => Cart(North, Position(m._2, l._2))
          case 'v' => Cart(South, Position(m._2, l._2))
          case '<' => Cart(West, Position(m._2, l._2))
          case '>' => Cart(East, Position(m._2, l._2))
          case _ => Empty
        }
      }
    }

    val carts: List[Cart] = trackPieces.collect{ case tp: Cart => tp }
    val fullTrackPieces = trackPieces.map {
      case Cart(d, p) if d == North | d == South => Straight(NorthSouth, p)
      case Cart(d, p) if d == East | d == West => Straight(EastWest, p)
      case t => t
    }

    (Track(fullTrackPieces), carts)
  }

}
