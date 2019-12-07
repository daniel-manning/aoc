package Year2018

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec


class Day13Spec extends AnyWordSpec with Matchers {
  val track =
    """/-----\
      ||     |
      ||  /--+--\
      ||  |  |  |
      |\--+--/  |
      |   |     |
      |   \-----/""".stripMargin


  val collisionTrack =
    """|
                          |v
       ||
                          ||
                          ||
                          |^
       ||""".stripMargin


  "Track parser" ignore {
    "be able to parse track" ignore {
      val expandedTrack = Track(List(Curve(Position(0, 0)), Straight(EastWest, Position(1, 0)), Straight(EastWest, Position(2, 0)), Straight(EastWest, Position(3, 0)), Straight(EastWest, Position(4, 0)), Straight(EastWest, Position(5, 0)), Curve(Position(6, 0)), Straight(NorthSouth, Position(0, 1)), Empty, Empty, Empty, Empty, Empty, Straight(NorthSouth, Position(6, 1)), Straight(NorthSouth, Position(0, 2)), Empty, Empty, Curve(Position(3, 2)), Straight(EastWest, Position(4, 2)), Straight(EastWest, Position(5, 2)), Intersection(Position(6, 2)), Straight(EastWest, Position(7, 2)), Straight(EastWest, Position(8, 2)), Curve(Position(9, 2)), Straight(NorthSouth, Position(0, 3)), Empty, Empty, Straight(NorthSouth, Position(3, 3)), Empty, Empty, Straight(NorthSouth, Position(6, 3)), Empty, Empty, Straight(NorthSouth, Position(9, 3)), Curve(Position(0, 4)), Straight(EastWest, Position(1, 4)), Straight(EastWest, Position(2, 4)), Intersection(Position(3, 4)), Straight(EastWest, Position(4, 4)), Straight(EastWest, Position(5, 4)), Curve(Position(6, 4)), Empty, Empty, Straight(NorthSouth, Position(9, 4)), Empty, Empty, Empty, Straight(NorthSouth, Position(3, 5)), Empty, Empty, Empty, Empty, Empty, Straight(NorthSouth, Position(9, 5)), Empty, Empty, Empty, Curve(Position(3, 6)), Straight(EastWest, Position(4, 6)), Straight(EastWest, Position(5, 6)), Straight(EastWest, Position(6, 6)), Straight(EastWest, Position(7, 6)), Straight(EastWest, Position(8, 6)), Curve(Position(9, 6))))

      val (parsedTrack: Track, carts) = TrackParser.parse(track)
      parsedTrack shouldBe expandedTrack
      carts shouldBe Nil
    }
  }


}
