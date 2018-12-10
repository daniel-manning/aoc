package Day05


object PolymerReactor {

  //val sameLetter =

  def areSameLetter(a:Char, b:Char):Boolean = a.toLower == b.toLower
  def areSameLetterButDifferentCase(a:Char, b:Char):Boolean = areSameLetter(a,b) && a != b


  def scanAndReactPolymer(polymerChain:String):String = {
    polymerChain.toList.sliding(2).map { case List(a, b) => (a, b) }
      .filterNot(p => areSameLetterButDifferentCase(p._1, p._2))
      .toList.mkString
  }

}
