package Year2019

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps

object Day14 extends App {
  val recipe: List[Reaction] = Source.fromResource("2019/day14")
    .getLines()
    .toList
    .map(s => ReactionParser.parse(ReactionParser.reaction, s).get)

  val oreIngredient = CriticalPath.minimumInput(recipe)

  println(s"To create 1 unit of FUEL we need ${oreIngredient.volume} units of ORE")
}


case class Ingredient(volume: Int, name: String)

case class Reaction(requirements: Seq[Ingredient], result: Ingredient)

object ReactionParser extends RegexParsers {
  def number: Parser[Int] = """[-0-9]+""".r ^^ { _.toInt }
  def label: Parser[String] = """[A-Z]+""".r ^^ { _.toString }
  def comma: Parser[String]    = """,""".r ^^ { _.toString }

  def arrow: Parser[String] = """=>""" ^^ { _.toString }

  def ingredient: Parser[Ingredient] = number ~ label ^^ {case n ~ l => Ingredient(n, l)}

  def reaction: Parser[Reaction] =
    (ingredient ~ (comma?) ).* ~ arrow ~ ingredient ^^ { case list ~ _  ~ result => Reaction(list.map(_._1), result) }
}



object CriticalPath {
  def makeReactionListFromRecipe(recipe: String): Seq[Reaction] =
    recipe.split("\n").map{ s =>
      ReactionParser.parse(ReactionParser.reaction, s).get
    }.toSeq

  def minimumInput(reactions: Seq[Reaction]): Ingredient = {

   val reactionMap = turnReactionsToMap(reactions)

    val ingredients = Seq(Ingredient(1, "FUEL"))

    @scala.annotation.tailrec
    def go(reactionGraph: Map[String, Seq[String]], ingredientList: Seq[Ingredient]): Seq[Ingredient] = {
      if(reactionGraph.isEmpty) ingredientList
      else {
        val (newIngredientList, newGraph) = breadthFirstSearch(reactionGraph, ingredientList, reactions)
        go(newGraph, newIngredientList)
      }
    }

    go(reactionMap, ingredients).find(_.name == "ORE").get
  }

  def decomposeTarget(name:String, ingredients: Seq[Ingredient], reactions: Seq[Reaction]): Seq[Ingredient] = {
    val targetReaction = reactions.find(_.result.name == name).get

    val requirements = targetReaction.requirements

    val targetVolume = targetReaction.result.volume

    val requiredVolume = ingredients.find(i => i.name == name).get.volume

    val totalReactants = if(requiredVolume <= targetVolume){
      requirements
    } else {
      val scalefactor = Math.ceil(requiredVolume.toDouble/targetVolume).toInt
      requirements.map(i => Ingredient(i.volume*scalefactor, i.name))
    }

    (ingredients.filterNot(_.name == name) ++ totalReactants).groupBy(_.name).map {
      l =>
        Ingredient(l._2.map(_.volume).sum, l._1)
    }.toSeq.sortBy(_.name)
  }

  def turnReactionsToMap(reactions: Seq[Reaction]):Map[String, Seq[String]] =
    reactions.map(l => (l.result.name, l.requirements.map(_.name))).toMap

  def breadthFirstSearch(graph: Map[String, Seq[String]], ingredients: Seq[Ingredient], reactions: Seq[Reaction]): (Seq[Ingredient], Map[String, Seq[String]]) = {
    val nextSearch = findEnds(graph)

    println(s"removing ends: $nextSearch")
    nextSearch.foldLeft((ingredients, graph)) {
      (newIngredients, name) =>
        (decomposeTarget(name, newIngredients._1, reactions), newIngredients._2 - name)
    }
  }

  def findEnds(graph: Map[String, Seq[String]]): Seq[String] = {
    val keys = graph.keySet
    val values = graph.values.flatten.toSeq

    keys.filterNot(values.contains(_)).toSeq
  }
}
