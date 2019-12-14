package Year2019

import scala.collection.Set
import scala.io.Source

object Day06 extends App {

  val orbitList = Source.fromResource("2019/day06")
  .getLines().toList

  val totalOrbits = OrbitMap.constructFromOrbitList(orbitList).totalOrbits

  println(s"Total number of orbits: $totalOrbits")

  val totalHops = OrbitMap
    .constructFromOrbitList(orbitList)
    .totalNumberOfGravityHopsBetweenTwoOrbitals(OrbitalObject("YOU"), OrbitalObject("SAN"))

  println(s"Total number of hops to get to Santa: $totalHops")

}



case class OrbitMap(orbits: Map[OrbitalObject, Set[OrbitalObject]]){
  def findStartNode: OrbitalObject = {
    val keys = this.orbits.keySet
    val values = this.orbits.values.fold(Set.empty)(_ ++ _)
    keys.diff(values).toList match {
      case Seq(node) => node
      case _ => throw new Exception("Non unique source for tree")
    }
  }

  def createDistanceMap():Map[OrbitalObject, Int] = {
    val startNode = findStartNode

    @scala.annotation.tailrec
    def calculate(orbitMap:  Map[OrbitalObject, Set[OrbitalObject]], distanceMap:Map[OrbitalObject, Int]): Map[OrbitalObject, Int] = {
      if(orbitMap.isEmpty) distanceMap
      else {
        val nodeSeq = MaxOps.maxBy(distanceMap)(_._2)
        val children:Map[OrbitalObject, Int] = nodeSeq.flatMap { p => if(orbitMap.get(p._1).isDefined) {orbitMap(p._1).map(a => a -> (p._2 + 1))} else {Map()} }.toMap
        calculate(orbitMap -- nodeSeq.map(_._1), distanceMap ++ children)
      }
    }
    calculate(orbits, Map(startNode -> 0))
  }

  def totalOrbits: Int = createDistanceMap().toList.map(_._2).sum

  def findPathToStartNode(begin: OrbitalObject):Seq[OrbitalObject] = {
    val startNode = findStartNode

    @scala.annotation.tailrec
    def go(nextNode: OrbitalObject, path: Seq[OrbitalObject]): Seq[OrbitalObject] = {
      if(nextNode == startNode) path
      else {
        val parent = orbits.filter(_._2 contains nextNode).head._1
        go(parent, path :+ parent)
      }
    }

    go(begin, Seq(begin))
  }

  def findPathBetweenTwoNodes(nodeOne:OrbitalObject, nodeTwo: OrbitalObject): Seq[OrbitalObject] = {
    val pathOne = findPathToStartNode(nodeOne)
    val pathTwo = findPathToStartNode(nodeTwo)

    val commonpath = pathOne.toSet.intersect(pathTwo.toSet).size

    pathOne.reverse.drop(commonpath - 1).reverse ++ pathTwo.reverse.drop(commonpath)
  }

  def totalNumberOfGravityHopsBetweenTwoOrbitals(nodeOne:OrbitalObject, nodeTwo: OrbitalObject): Int =
    findPathBetweenTwoNodes(nodeOne, nodeTwo).length - 3
}

object OrbitMap {
  def constructFromOrbitList(source: Seq[String]): OrbitMap = {
    source.foldLeft(OrbitMap(Map.empty[OrbitalObject, Set[OrbitalObject]])) {
      (orbits, orbit) =>
        val (parent, child) = OrbitalObject.parseOrbitRelation(orbit)
        val newChildren: Set[OrbitalObject] = orbits.orbits.get(OrbitalObject(parent)) match {
          case Some(children) => children + OrbitalObject(child)
          case None => Set(OrbitalObject(child))
        }

        OrbitMap((orbits.orbits - OrbitalObject(parent)).updated(OrbitalObject(parent), newChildren))
    }
  }
}


case class OrbitalObject( ObjectName: String)

object OrbitalObject {
  def parseOrbitRelation(orbit:String):(String, String) = {
    orbit.split("\\)").toList match{
      case Seq(parent, child) => (parent, child)
    }
  }
}
