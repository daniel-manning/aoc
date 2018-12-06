package Day04

import java.text.SimpleDateFormat
import java.util.Date

case class Interval(startTime:Int, endTime:Int)

case class GuardDuty(day:Date, guardID:String, timesAsleep: Seq[Interval])

object Day04 {

  val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm")
  val loglinePattern = "\\[([0-9\\-: ]+)\\] ([a-zA-Z0-9# ]+)".r
  val guardIDPattern = "Guard #([0-9]+) begins shift".r

  def marshallGuardDuty(logLines:Seq[String]):Seq[GuardDuty] = {

    val marshalledLines = logLines.map(l => loglinePattern.findFirstMatchIn(l).map( mat => (sdf.parse(mat.group(1)), mat.group(2))).get)
    val actions:Map[Int, Seq[(Date, String)]] = marshalledLines.groupBy(l => if(l._1.getHours == 23){l._1.getDate + 1 }else{l._1.getDate})
    actions.map { p =>
      buildGuardDuty(p._2)
    }.toSeq
  }


  def buildGuardDuty(logs:Seq[(Date, String)]):GuardDuty = {
       def wakeUpCycle(logTail:Seq[(Date, String)]):List[Interval] = {
       	  if(logTail.isEmpty){
		        List()
	        }else{
		        val interval = Interval(logTail.head._1.getMinutes, logTail.tail.head._1.getMinutes - 1)
            (interval) :: wakeUpCycle(logTail.drop(2))
	        }
	     }

    val guardID = guardIDPattern.findFirstMatchIn(logs.head._2).map(_.group(1)).get
    GuardDuty(logs.head._1, guardID, wakeUpCycle(logs.tail))
  }

  def chooseGuard(logLines:Seq[String]):String = {
     val guardDuties = marshallGuardDuty(logLines)

     val guardsAsleep = guardDuties.map(_.guardID).distinct.map(id => (id, guardDuties.filter(_.guardID == id).map(_.timesAsleep.map((in:Interval) => in.endTime - in.startTime + 1).sum).sum))
    guardsAsleep.maxBy(_._2)._1
  }

  def chosenMinute(logs:Seq[(Date, String)], guardChosen:String):Int = {
    val guardSleepIntervals = marshallGuardDuty(logLines).filter(_.guardID == guardChosen).map(_.timesAsleep)
    
  }

}
