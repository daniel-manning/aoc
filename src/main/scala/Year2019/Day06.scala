package Year2019

object Day06 extends App {

}


case class OrbitalObject( ObjectName: String, children: Seq[OrbitalObject])

object OrbitalObject {

  def constructFromOrbitList(source: Seq[String]): OrbitalObject = {
    val orbit = source.head
    val (parent, child) = parseOrbitRelation(orbit)
    val root = OrbitalObject(parent, Seq(OrbitalObject(child, Nil)))

    source.tail.foldLeft(root){
      (orbitMap, orbit) =>
        val (parent, child) = parseOrbitRelation(orbit)
        val newChild = OrbitalObject(child, Nil)

        val parentRoot = findParent(parent, orbitMap)
    }
  }

  private def parseOrbitRelation(orbit:String):(String, String) = {
    orbit.split("\\)").toList match{
      case Seq(parent, child) => (parent, child)
    }
  }

}

