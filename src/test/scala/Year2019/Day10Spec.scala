package Year2019

import models.Position
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
      val result = Seq(
        Position(2,2), Position(4,4), Position(4,0), Position(1,0), Position(0,2), Position(1,2), Position(4,3), Position(3,2), Position(4,2), Position(3,4)
      )

      AsteroidField.construct(fieldString.split("\n").toIndexedSeq) shouldBe AsteroidField(result)
    }

    "be able to calculate the asteroids visible from" in {
      val field = AsteroidField.construct(fieldString.split("\n").toIndexedSeq)
      field.noOfAsteroidsVisibleFromPosition(Position(3,4)) shouldBe 8
    }

    "be able to correctly score positions for asteroid base" in {
      val result = Seq((Position(0,2),6), (Position(1,0),7), (Position(1,2),7), (Position(2,2),7), (Position(3,2),7),
        (Position(3,4),8), (Position(4,0),7), (Position(4,2),5), (Position(4,3),7), (Position(4,4),7))
      val field = AsteroidField.construct(fieldString.split("\n").toIndexedSeq)
      field.scorePositionsForAsteroidBase shouldBe result
    }

    "be able to find the best position for asteroid base" in {
      val field = AsteroidField.construct(fieldString.split("\n").toIndexedSeq)
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

      val field = AsteroidField.construct(testField.split("\n").toIndexedSeq)
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

      val field = AsteroidField.construct(testField.split("\n").toIndexedSeq)
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

      val field = AsteroidField.construct(testField.split("\n").toIndexedSeq)
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

      val field = AsteroidField.construct(testField.split("\n").toIndexedSeq)
      field.findBestPositionForAsteroidBase shouldBe (Position(11,13), 210)
    }

    "correctly identify asteroids blasted by one revolution of laser cannon" in {
      val testField = """.#....#####...#..
                        |##...##.#####..##
                        |##...#...#.#####.
                        |..#.....#...###..
                        |..#.#.....#....##""".stripMargin

      val result = Seq(
        (Position(8,1),1), (Position(9,0),2), (Position(9,1),3), (Position(10,0),4),
        (Position(9,2),5), (Position(11,1),6), (Position(12,1),7), (Position(11,2),8),
        (Position(15,1),9), (Position(12,2),10), (Position(13,2),11), (Position(14,2),12),
        (Position(15,2),13), (Position(12,3),14), (Position(16,4),15), (Position(15,4),16),
        (Position(10,4),17), (Position(4,4),18), (Position(2,4),19), (Position(2,3),20),
        (Position(0,2),21), (Position(1,2),22), (Position(0,1),23), (Position(1,1),24),
        (Position(5,2),25), (Position(1,0),26), (Position(5,1),27), (Position(6,1),28),
        (Position(6,0),29), (Position(7,0),30)
      )

      val field = AsteroidField.construct(testField.split("\n").toIndexedSeq)
      field.oneRevolutionOfLaserBlaster(Position(8,3)) shouldBe result
    }

    "correctly identify asteroids blasted by second revolution of laser cannon" in {
      val result = Seq(
        (Position(8,0),1), (Position(10,1),2),
        (Position(14,0),3), (Position(16,1),4), (Position(13,3),5)
      )

      val leftOver = Seq(
        Position(8,0), Position(10,1),
        Position(14,0), Position(16,1), Position(13,3), Position(14,3)
      )

     AsteroidField(leftOver).oneRevolutionOfLaserBlaster(Position(8,3)) shouldBe result
    }

    "correctly identify order of all asteroids blasted" in {
      val testField = """.#....#####...#..
                        |##...##.#####..##
                        |##...#...#.#####.
                        |..#.....#...###..
                        |..#.#.....#....##""".stripMargin

      val result = Seq(
        (Position(8,1),1), (Position(9,0),2), (Position(9,1),3), (Position(10,0),4),
        (Position(9,2),5), (Position(11,1),6), (Position(12,1),7), (Position(11,2),8),
        (Position(15,1),9), (Position(12,2),10), (Position(13,2),11), (Position(14,2),12),
        (Position(15,2),13), (Position(12,3),14), (Position(16,4),15), (Position(15,4),16),
        (Position(10,4),17), (Position(4,4),18), (Position(2,4),19), (Position(2,3),20),
        (Position(0,2),21), (Position(1,2),22), (Position(0,1),23), (Position(1,1),24),
        (Position(5,2),25), (Position(1,0),26), (Position(5,1),27), (Position(6,1),28),
        (Position(6,0),29), (Position(7,0),30), (Position(8,0),31), (Position(10,1),32),
        (Position(14,0),33), (Position(16,1),34), (Position(13,3),35), (Position(14,3),36)
      )

      val field = AsteroidField.construct(testField.split("\n").toIndexedSeq)
      AsteroidField.removeAsteroidsWithLaserBlaster(Position(8,3), field) shouldBe result
    }


    "correctly identify order of all asteroids blasted in large test case" in {
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

      val field = AsteroidField.construct(testField.split("\n").toIndexedSeq)
      val blastingOrder = AsteroidField.removeAsteroidsWithLaserBlaster(Position(11,13), field)

      blastingOrder(0)._1 shouldBe Position(11,12)
      blastingOrder(1)._1 shouldBe Position(12,1)
      blastingOrder(2)._1 shouldBe Position(12,2)
      blastingOrder(9)._1 shouldBe Position(12,8)
      blastingOrder(19)._1 shouldBe Position(16,0)
      blastingOrder(49)._1 shouldBe Position(16,9)
      blastingOrder(99)._1 shouldBe Position(10,16)
      blastingOrder(198)._1 shouldBe Position(9,6)
      blastingOrder(199)._1 shouldBe Position(8,2)
      blastingOrder(200)._1 shouldBe Position(10,9)
      blastingOrder(298)._1 shouldBe Position(11,1)
    }
  }
}
