package Day04

import java.util.Date

case class Interval(startTime:Int, endTime:Int)

case class GuardDuty(day:Date, guardID:String, timesAsleep: Seq[Interval])

object Day04 {

  val loglineRegex = "[([0-9-:]+)] ([a-zA-Z0-9#])".r

  def marshallGuardDuty(logLines:Seq[String]):Seq[GuardDuty] = {



  }



  def chooseGuard() = {

  }

}
