package Year2019

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class Day10Spec extends AnyWordSpec with Matchers {

  val fieldString =
    """.#..#
      |.....
      |#####
      |....#
      |...##""".stripMargin

  "AsteroidField" should {
    "construct from String" in {
      val result = Map(
        Position(1,0) -> Asteroid, Position(4,0) -> Asteroid,
        Position(0,2) -> Asteroid, Position(1,2) -> Asteroid, Position(2,2) -> Asteroid, Position(3,2) -> Asteroid, Position(4,2) -> Asteroid,
        Position(4,3) -> Asteroid,
        Position(3,4) -> Asteroid, Position(4,4) -> Asteroid
      )

      AsteroidField.construct(fieldString.split("\n")) shouldBe AsteroidField(result)
    }

    "be able to calculate the asteroids visible from" in {
      val field = AsteroidField.construct(fieldString.split("\n"))
      field.noOfAsteroidsVisibleFromPosition(Position(3,4)) shouldBe 8
    }

    "be able to correctly score positions for asteroid base" in {
      val result = Seq((Position(0,2),6), (Position(1,0),7), (Position(1,2),7), (Position(2,2),7), (Position(3,2),7),
        (Position(3,4),8), (Position(4,0),7), (Position(4,2),5), (Position(4,3),7), (Position(4,4),7))
      val field = AsteroidField.construct(fieldString.split("\n"))
      field.scorePositionsForAsteroidBase shouldBe result
    }

    "be able to find the best position for asteroid base" in {
      val field = AsteroidField.construct(fieldString.split("\n"))
      field.findBestPositionForAsteroidBase shouldBe (Position(3,4), 8)
    }

    "be able to match test case one" in {
      val testField =
          """......#.#.
            |#..#.#....
            |..#######.
            |.#.#.###..
            |.#..#.....
            |..#....#.#
            |#..#....#.
            |.##.#..###
            |##...#..#.
            |.#....####""".stripMargin

      val field = AsteroidField.construct(testField.split("\n"))
      field.findBestPositionForAsteroidBase shouldBe (Position(5,8), 33)
    }

    "be able to match test case two" in {
      val testField =
        """#.#...#.#.
          |.###....#.
          |.#....#...
          |##.#.#.#.#
          |....#.#.#.
          |.##..###.#
          |..#...##..
          |..##....##
          |......#...
          |.####.###.""".stripMargin

      val field = AsteroidField.construct(testField.split("\n"))
      field.findBestPositionForAsteroidBase shouldBe (Position(1,2), 35)
    }

    "be able to match test case three" in {
      val testField =
        """.#..#..###
          |####.###.#
          |....###.#.
          |..###.##.#
          |##.##.#.#.
          |....###..#
          |..#.#..#.#
          |#..#.#.###
          |.##...##.#
          |.....#.#..""".stripMargin

      val field = AsteroidField.construct(testField.split("\n"))
      field.findBestPositionForAsteroidBase shouldBe (Position(6,3), 41)
    }

    "be able to match test case four" in {
      val testField =
        """.#..##.###...#######
          |##.############..##.
          |.#.######.########.#
          |.###.#######.####.#.
          |#####.##.#.##.###.##
          |..#####..#.#########
          |####################
          |#.####....###.#.#.##
          |##.#################
          |#####.##.###..####..
          |..######..##.#######
          |####.##.####...##..#
          |.#####..#.######.###
          |##...#.##########...
          |#.##########.#######
          |.####.#.###.###.#.##
          |....##.##.###..#####
          |.#.#.###########.###
          |#.#.#.#####.####.###
          |###.##.####.##.#..##""".stripMargin

      val field = AsteroidField.construct(testField.split("\n"))
      field.findBestPositionForAsteroidBase shouldBe (Position(11,13), 210)
    }

    "correctly identify asteroids blasted by one revolution of laser cannon" ignore {
      val testField = """.#....#####...#..
                        |##...##.#####..##
                        |##...#...#.#####.
                        |..#.....#...###..
                        |..#.#.....#....##""".stripMargin

      val result = Seq(
        (Position(8,1),1), (Position(9,0),2), (Position(9,1),3), (Position(10,0),4),
        (Position(9,2),5), (Position(11,1),6), (Position(12,1),7), (Position(11,2),8),
        (Position(14,1),9))

      val field = AsteroidField.construct(testField.split("\n"))
      field.oneRevolutionOfLaserBlaster(Position(8,3)) shouldBe result
    }
  }


  "Rational" should {
    "reduction fractions to their lowest term" in {
      Rational(4, 12).lowestTerms shouldBe Rational(1, 3)
    }

    "add two rationals" in {
      Rational(1, 2) + Rational(1, 4) shouldBe Rational(3, 4)
    }

    "multiply two rationals" in {
      Rational(1, 2) * Rational(1, 4) shouldBe Rational(1, 8)
    }
  }
}
