package Day04

import java.text.SimpleDateFormat
import java.util.Date
import scala.io.Source

case class Interval(startTime:Int, endTime:Int)

case class GuardDuty(day:Date, guardID:String, timesAsleep: Seq[Interval])

object Day04Runner extends App {

val logLines = Source.fromResource("day04_input").getLines.toList
val guard = Day04.chooseGuard(logLines)
            Day04.chosenMinute(logLines, guard)
}


object Day04 {

  val sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm")
  val loglinePattern = "\\[([0-9\\-: ]+)\\] ([a-zA-Z0-9# ]+)".r
  val guardIDPattern = "Guard #([0-9]+) begins shift".r

  def marshallGuardDuty(logLines:List[String]):List[GuardDuty] = {

    val marshalledLines = logLines.map(l => loglinePattern.findFirstMatchIn(l).map( mat => (sdf.parse(mat.group(1)), mat.group(2))).get)
    val actions:Map[Int, List[(Date, String)]] = marshalledLines.groupBy(l => if(l._1.getHours == 23){l._1.getDate + 1 }else{l._1.getDate})
    actions.map { p =>
      buildGuardDuty(p._2)
    }.toList
  }


  def buildGuardDuty(logs:List[(Date, String)]):GuardDuty = {
       def wakeUpCycle(logTail:List[(Date, String)]):List[Interval] = {
       	  if(logTail.isEmpty){
		        List()
	        }else{
		        val interval = Interval(logTail.head._1.getMinutes, logTail.tail.head._1.getMinutes - 1)
            (interval) :: wakeUpCycle(logTail.drop(2))
	        }
	     }
    println(s"matching: ${logs.head._2}")
    val guardID = guardIDPattern.findFirstMatchIn(logs.head._2).map(_.group(1)).get
    GuardDuty(logs.head._1, guardID, wakeUpCycle(logs.tail))
  }

  def chooseGuard(logLines:List[String]):String = {
     val guardDuties = marshallGuardDuty(logLines)

     val guardsAsleep = guardDuties.map(_.guardID).distinct.map(id => (id, guardDuties.filter(_.guardID == id).map(_.timesAsleep.map((in:Interval) => in.endTime - in.startTime + 1).sum).sum))
    guardsAsleep.maxBy(_._2)._1
  }

  def chosenMinute(logs:List[String], guardChosen:String):Unit = {
    val guardSleepIntervals = marshallGuardDuty(logs).filter(_.guardID == guardChosen).flatMap(_.timesAsleep)
    
    println(guardSleepIntervals)
    val endResult = findInteralIntersections(guardSleepIntervals)
    println(endResult)
    ???
  }

  def findInteralIntersections(intervals:List[Interval]):List[Interval] = {
     def compare(intervals:List[Interval]):List[Interval] = {
      if(intervals.isEmpty){
         List[Interval]()
      }else{
       println(s"intervals: ${intervals.tail.map(interval => intersectInterval(intervals.head, interval)).filter(_.isDefined)}")
       val others = intervals.tail.map( interval => intersectInterval(intervals.head, interval)).filter(_.isDefined).map(_.get)
       others ++  compare(intervals.tail)
      }
     }
    
     compare(intervals)
  }

  def intersectInterval(intervalOne:Interval, intervalTwo:Interval):Option[Interval] = {
    if(intervalOne.startTime > intervalTwo.startTime) {
           intersectInterval(intervalTwo, intervalOne)
    }else{
          if(intervalTwo.startTime <= intervalOne.endTime){ 
             Some(Interval(intervalTwo.startTime, Math.min(intervalOne.endTime, intervalTwo.endTime)))
	  }else{
           None
          }
    }
  }


}
