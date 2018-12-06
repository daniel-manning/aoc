package Day04

import java.text.{DateFormat, SimpleDateFormat}
import java.util.Date

import org.scalatest.{Matchers, WordSpec}

class Day04Spec extends WordSpec with Matchers {

  val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm")

  val schedule = """[1518-11-01 00:00] Guard #10 begins shift
    |[1518-11-01 00:05] falls asleep
    |[1518-11-01 00:25] wakes up
    |[1518-11-01 00:30] falls asleep
    |[1518-11-01 00:55] wakes up
    |[1518-11-01 23:58] Guard #99 begins shift
    |[1518-11-02 00:40] falls asleep
    |[1518-11-02 00:50] wakes up
    |[1518-11-03 00:05] Guard #10 begins shift
    |[1518-11-03 00:24] falls asleep
    |[1518-11-03 00:29] wakes up
    |[1518-11-04 00:02] Guard #99 begins shift
    |[1518-11-04 00:36] falls asleep
    |[1518-11-04 00:46] wakes up
    |[1518-11-05 00:03] Guard #99 begins shift
    |[1518-11-05 00:45] falls asleep
    |[1518-11-05 00:55] wakes up""".stripMargin.lines.toList


  "Guard Chooser" should {
    "marshall the data correctly" in {

      val marshalledDuties = Day04.marshallGuardDuty(schedule).sortBy(_.day)

      val duties = Seq(
        GuardDuty(sdf.parse("1518-11-01 00:00"), "10", Seq(Interval(5, 24), Interval(30, 54))),
        GuardDuty(sdf.parse("1518-11-01 23:58"), "99", Seq(Interval(40, 49))),
        GuardDuty(sdf.parse("1518-11-03 00:05"), "10", Seq(Interval(24, 28))),
        GuardDuty(sdf.parse("1518-11-04 00:02"), "99", Seq(Interval(36, 45))),
        GuardDuty(sdf.parse("1518-11-05 00:03"), "99", Seq(Interval(45, 54))),
      )

        marshalledDuties shouldBe duties
    }

    "choose the correct guard asleep" in {
      val guard = Day04.chooseGuard(schedule)

      guard shouldBe "10"
    }

    "choose the correct minute" in {
      val minute = Day04.chosenMinute(schedule, "10")

      minute shouldBe 24
    }

    "intersect two intervals correctly" in {
    val intervalOne = Interval(24, 30)
    val intervalTwo = Interval(29, 40)

    val result = Interval(29,30)
    Day04.intersectInterval(intervalOne, intervalTwo).get shouldBe result 
   }
  }


}
