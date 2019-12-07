package Year2018

import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoField._
import java.time.{LocalDateTime, ZoneOffset}

import scala.io.Source

case class Interval(startTime:Int, endTime:Int){
  def isSinglePoint:Boolean = startTime == endTime
}

case class GuardDuty(day:LocalDateTime, guardID:String, timesAsleep: Seq[Interval])

object Day04Runner extends App {

  val logLines = Source.fromResource("2018/day04_input").getLines.toList
  val guard = Day04.chooseGuard(logLines)
  val min = Day04.chosenMinute(logLines, guard)
  println(s"output is $guard * $min = ${guard.toInt*min}")
}

object Day04_02_Runner extends App {

  val logLines = Source.fromResource("2018/day04_input").getLines.toList
  val (guard, min) = Day04.findMostPopularMin(Day04.marshallGuardDuty(logLines))
  println(s"output is $guard * $min = ${guard.toInt*min}")
}

object Day04 {

  implicit val localDateOrdering: Ordering[LocalDateTime] = Ordering.by(_.toEpochSecond(ZoneOffset.UTC))

  val df: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")
  val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
  val loglinePattern = "\\[([0-9\\-: ]+)\\] ([a-zA-Z0-9# ]+)".r
  val guardIDPattern = "Guard #([0-9]+) begins shift".r

  def marshallGuardDuty(logLines:List[String]):List[GuardDuty] = {

    val marshalledLines = logLines.map(l => loglinePattern.findFirstMatchIn(l).map( mat => (LocalDateTime.parse(mat.group(1), df), mat.group(2))).get)
    val actions:Map[String, List[(LocalDateTime, String)]] = marshalledLines.sortBy(_._1).groupBy(l => if(l._1.get(CLOCK_HOUR_OF_DAY) == 23){ formatter.format(l._1.plusDays(1))}else{formatter.format(l._1)})
    //println(s"Marshalled Order: ${actions}")
    actions.map { p =>
      //println(s"matching( ${p._1} ): ${p._2.head._2}")
      buildGuardDuty(p._2)
    }.toList
  }


  def buildGuardDuty(logs:List[(LocalDateTime, String)]):GuardDuty = {
       def wakeUpCycle(logTail:List[(LocalDateTime, String)]):List[Interval] = {
       	  if(logTail.isEmpty){
		        List()
	        }else{
		        val interval = Interval(logTail.head._1.get(MINUTE_OF_HOUR), logTail.tail.head._1.get(MINUTE_OF_HOUR) - 1)
            (interval) :: wakeUpCycle(logTail.drop(2))
	        }
	     }

    val guardID = guardIDPattern.findFirstMatchIn(logs.head._2).map(_.group(1)).get
    GuardDuty(logs.head._1, guardID, wakeUpCycle(logs.tail))
  }

  def chooseGuard(logLines:List[String]):String = {
     val guardDuties = marshallGuardDuty(logLines)

     val guardsAsleep = guardDuties.map(_.guardID).distinct.map(id => (id, guardDuties.filter(_.guardID == id).map(_.timesAsleep.map((in:Interval) => in.endTime - in.startTime + 1).sum).sum))

    guardsAsleep.maxBy(_._2)._1
  }

  def chosenMinute(logs:List[String], guardChosen:String):Int = {
    val guardSleepIntervals = marshallGuardDuty(logs).filter(_.guardID == guardChosen).flatMap(_.timesAsleep)
    
    val endResult = iterateIntervalIntersections(guardSleepIntervals)
    endResult.head.startTime
  }

  def iterateIntervalIntersections(list:List[Interval]):List[Interval] = {
    if(list.size <= 1 || list.forall(_.isSinglePoint)){
      list.distinct
    }else {
      iterateIntervalIntersections(findInteralIntersections(list))
    }
  }

  def findInteralIntersections(intervals:List[Interval]):List[Interval] = {
     def compare(intervals:List[Interval]):List[Interval] = {
      if(intervals.isEmpty){
         List[Interval]()
      }else{
       //println(s"intervals: ${intervals.tail.map(interval => intersectInterval(intervals.head, interval)).filter(_.isDefined)}")
       val others = intervals.tail.map( interval => intersectInterval(intervals.head, interval)).filter(_.isDefined).map(_.get)
       others.distinct ++  compare(intervals.tail).distinct
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

  def findMostPopularMin(guardDuties:List[GuardDuty]):(String, Int) = {
    val result = guardDuties.flatMap{ gd => gd.timesAsleep.flatMap( interval => (interval.startTime to interval.endTime).map((gd.guardID, _)))}.groupBy(identity).map(a => (a._1, a._2.size))

    result.maxBy(_._2)._1
  }


}
