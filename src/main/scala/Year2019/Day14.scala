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

  val maxFuelForOre = CriticalPath.calculateYieldForFuel(recipe, Ingredient(BigInt("1000000000000"), "ORE"))
  println(s"Maximum fuel for ore: $maxFuelForOre")
}


case class Ingredient(volume: BigInt, name: String)

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
  def calculateYieldForFuel(reactions: Seq[Reaction], oreAmount: Ingredient): BigInt = {
    val orePerFuel = minimumInput(reactions)
    val firstApproximation = oreAmount.volume / orePerFuel.volume
    val exponent = firstApproximation.toString.length

       @scala.annotation.tailrec
       def iterateUntilOverEstimate(value: BigInt, step:BigInt, limit: BigInt): BigInt = {
         val total = minimumInput(reactions, value).volume
         println(s"value: $value, step: $step, limit: $limit, total: $total")
         if(total > limit){
           if(step == BigInt(1)) value - step
           else iterateUntilOverEstimate(value - step, (step/BigInt(10)), limit)
         } else {
           iterateUntilOverEstimate(value + step, step, limit)
         }
       }

    iterateUntilOverEstimate(firstApproximation, BigInt(10).pow(exponent - 1), oreAmount.volume)
  }

  def makeReactionListFromRecipe(recipe: String): Seq[Reaction] =
    recipe.split("\n").map{ s =>
      ReactionParser.parse(ReactionParser.reaction, s).get
    }.toSeq

  def minimumInput(reactions: Seq[Reaction], forFuel: BigInt = BigInt(1)): Ingredient = {

   val reactionMap = turnReactionsToMap(reactions)

    val ingredients = Seq(Ingredient(forFuel, "FUEL"))

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

  def results(reactions: Seq[Reaction]): Set[Ingredient] = {
    val ore = minimumInput(reactions)
    val ingredient = Set(ore)

    @scala.annotation.tailrec
    def go(reactions: Seq[Reaction], ingredients: Set[Ingredient]):Set[Ingredient] = {
      val all = findEachAvailableReaction(reactions, ingredients)
      println(s"all: $all")
      val updatedIngredients = useRecipe(all, ingredients)
      println(s"updatedIngredients: $updatedIngredients")

      if(updatedIngredients.exists(_.name == "FUEL")) updatedIngredients
      else go(reactions, updatedIngredients)
    }

    go(reactions, ingredient)
  }


  def useRecipe(reactions: Seq[Reaction], ingredients: Set[Ingredient]): Set[Ingredient] =
    reactions.foldRight(ingredients)((a, b) =>  useRecipe(a, reactions, b))

  def useRecipe(reaction: Reaction, reactions: Seq[Reaction], ingredients: Set[Ingredient]): Set[Ingredient] = {
    println(s"Trying to run reaction: $reaction")
    val list = reaction.requirements.foldLeft((ingredients.toSet)) {
      (a, b) =>
        println(s"a: $a")
        println(s"b: $b")
        val ingredient = a.find(_.name == b.name).get
        val removed = a.removedAll(Set(ingredient))
        if (ingredient.volume == b.volume)
          removed
        else if (ingredient.volume < b.volume) {
          //we need more reaction ingredients to satisfy current reactions
          val reactionIngredientToBoost = reactions.find(_.result.name == ingredient.name).get
          val newIngredients = useRecipe(reactionIngredientToBoost, reactions, a)
          println(s"newIngredients: $newIngredients")
          val newIngredient = newIngredients.find(_.name == b.name).get

          val newPot = newIngredients.removedAll(Set(newIngredient)).union(Set(Ingredient(newIngredient.volume - b.volume, b.name)))
          println(s"New reacted pot: $newPot")
          newPot
        }
        else
          removed.union(Set(Ingredient(ingredient.volume - b.volume, b.name)))
    }

    //add reaction to ingredients adding in new
    list.find(_.name == reaction.result.name)
      .map(i => list.removedAll(Set(i)).union(Set(Ingredient(reaction.result.volume + i.volume, i.name))))
      .getOrElse(list.union(Set(reaction.result)))
  }

  def findEachAvailableReaction(reactions:Seq[Reaction], ingredients: Set[Ingredient]): Seq[Reaction] =
    reactions.filter(r => /*!ingredients.exists(_.name == r.result.name) &&*/ r.requirements.forall(i => ingredients.exists(_.name == i.name)))

  def decomposeTarget(name:String, ingredients: Seq[Ingredient], reactions: Seq[Reaction]): Seq[Ingredient] = {
    val targetReaction = reactions.find(_.result.name == name).get

    val requirements = targetReaction.requirements

    val targetVolume = targetReaction.result.volume

    val requiredVolume = ingredients.find(i => i.name == name).get.volume

    val totalReactants = if(requiredVolume <= targetVolume){
      requirements
    } else {
      val scalefactor = Math.ceil((BigDecimal(requiredVolume)/BigDecimal(targetVolume)).toDouble).toInt
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

    //println(s"removing ends: $nextSearch")
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
