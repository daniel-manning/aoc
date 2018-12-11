package Day05

import scala.annotation.tailrec
import scala.io.Source


object Day05 extends App {
  val polymerChain = Source.fromResource("day05_input").getLines.toList.head
  println(s"String has started at ${polymerChain.length}")
  val reactedPolymer = PolymerReactor.scanAndReactPolymer(polymerChain)
  println(s"String has gone from ${polymerChain.length} characters to ${reactedPolymer.length} characters: $reactedPolymer")
}

object Day05_02 extends App {
  //val polymerChain = "dabCBAcaDA"
  val polymerChain = Source.fromResource("day05_reduced").getLines.toList.head
  val incidences = polymerChain.map(_.toLower).groupBy(identity).map(p => (p._1, p._2.length))
  def unreactedWithBlocker(char:Char):Int = polymerChain.sliding(3).toList.count(a => a.toList match {
    case List(b, c, d) if c.toLower == char => PolymerReactor.areSameLetterButDifferentCase(b, d)
    case List(b, c, d) => false
    case _ => false
  })


  println(incidences)
  println(s"unreactedwithblocker: ${incidences.toList.map(a => (a._1, unreactedWithBlocker(a._1))).sortBy(a => a._2)}")

  val replacedPolymers = incidences.toList.map(a => (a._1, replaceAndReactLength(a._1)))

  def replaceAndReactLength(char:Char):Int = {
    val newPolymer = polymerChain.replace(""+char.toLower, "").replace(""+char.toUpper, "")
    val newCount = PolymerReactor.countUnreactedPolymers(newPolymer)
    println(s"When we replace $char: $newCount")

    newCount
  }

  println(s"replacedPolymers: $replacedPolymers")

}

object PolymerReactor {

  //val sameLetter =

  def areSameLetter(a:Char, b:Char):Boolean = a.toLower == b.toLower
  def areSameLetterButDifferentCase(a:Char, b:Char):Boolean = areSameLetter(a,b) && a != b


  def scanAndReactPolymer(polymerChain:String):String = {
    @tailrec
    def filterByPairs(poly:List[Char], accum:List[Char]):String = {
      //println(s"filterByPairs - ${poly.size} - ${accum.length}")
      if (poly.isEmpty) {
        accum.reverse.mkString
      }else if(poly.length == 1){
        filterByPairs(poly.tail, poly.head :: accum)
      }else if(areSameLetterButDifferentCase(poly.head, poly.tail.head)){
        filterByPairs(poly.tail.tail, accum)
      }else{
        filterByPairs(poly.tail, poly.head :: accum)
      }
    }

    val reactedPolymer = filterByPairs(polymerChain.toList, List[Char]())

    println(s"reactedPolymer length: ${reactedPolymer.length}")

    if(reactedPolymer == polymerChain){
      reactedPolymer.mkString
    }else{
      scanAndReactPolymer(reactedPolymer.mkString)
    }
  }

  def countUnreactedPolymers(polymerChain:String):Int = {
    scanAndReactPolymer(polymerChain).length
  }

}
