package Year2019

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers
import scala.language.postfixOps
import scala.math.BigDecimal.RoundingMode

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

sealed trait ReactionResult
case object ReactionSuccess extends ReactionResult
case object ReactionFailure extends ReactionResult


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

  def results(reactions: Seq[Reaction]): (BigInt, Set[Ingredient]) = {
    val ore = minimumInput(reactions)
    val ingredient = Set(ore)

    @scala.annotation.tailrec
    def go(reactions: Seq[Reaction], ingredients: Set[Ingredient]):Set[Ingredient] = {
      val all = findEachAvailableReaction(reactions, ingredients)
      //println(s"all: $all")
      val (updatedIngredients, result) = useRecipe(all, reactions, ingredients)
      //println(s"updatedIngredients: $updatedIngredients")

      if(updatedIngredients.exists(_.name == "FUEL")) updatedIngredients
      else go(reactions, updatedIngredients)
    }

    (ore.volume, go(reactions, ingredient))
  }

  def totalThenSquash(reactions: Seq[Reaction], amountOfOre: BigInt): List[Ingredient] = {
    val (orePerFuel, oneFuelPlusWaste) = results(reactions)
    val scaleFactor = (BigDecimal(amountOfOre)/BigDecimal(orePerFuel)).setScale(0, RoundingMode.FLOOR).toBigInt //BigInt("100000")
    val remainder = amountOfOre.mod(orePerFuel) //amountOfOre - scaleFactor * orePerFuel

    val total = oneFuelPlusWaste
      .map(i => i.copy(volume = i.volume * scaleFactor))
      .union(Set(Ingredient(remainder, "ORE")))

    println("Hello --- A !!!")
    println(s"total: $total")
    LazyList.unfold(total){
      mixture =>
        println("Hello --- B !!!")
        val fuel = mixture.find(_.name == "FUEL")
        val pureWaste = mixture.filterNot(_.name == "FUEL")
        println(s"fuel: $fuel")
        println("Hello --- C !!!")
        val all = findEachAvailableReaction(reactions, pureWaste)
        println(s"all: $all")
        val updatedIngredients = useRecipe(all, reactions, pureWaste)
        println(s"updatedIngredients: $updatedIngredients")

       updatedIngredients._2 match {
         case ReactionSuccess => Some(fuel, updatedIngredients._1)
         case ReactionFailure => None
       }
    }.flatten.toList
  }


  def useRecipe(reactions: Seq[Reaction], allReactions: Seq[Reaction], ingredients: Set[Ingredient]): (Set[Ingredient], ReactionResult) =
    reactions.foldRight[(Set[Ingredient], ReactionResult)]((ingredients, ReactionFailure)){
      (a, b) =>  val result = useRecipe(a, allReactions, b._1)
        (result._1, if(result._2 == ReactionSuccess) ReactionSuccess else b._2)
    }

  def useRecipe(reaction: Reaction, reactions: Seq[Reaction], ingredients: Set[Ingredient]): (Set[Ingredient], ReactionResult) = {
    println(s"Trying to run reaction: $reaction")
    val list: (Set[Ingredient], ReactionResult) = reaction.requirements.foldLeft[(Set[Ingredient], ReactionResult)]((ingredients, ReactionFailure)) {
      (a, b) =>
        println(s"a: $a")
        println(s"b: $b")
        val ingredient = a._1.find(_.name == b.name).getOrElse(Ingredient(0, b.name))
        val removed = a._1.removedAll(Set(ingredient))
        if (ingredient.volume == b.volume)
          (removed, ReactionSuccess)
        else if (ingredient.volume < b.volume) {
          //println(s"We need more ingredients because ${ingredient} is less than $b")
          //if we're out of ore fail the experiment
          if(ingredient.name == "ORE") (a._1, ReactionFailure)
          else {
            //we need more reaction ingredients to satisfy current reactions
            val reactionIngredientToBoost = reactions.find(_.result.name == ingredient.name).get
             //boost reaction to scale we need
             val scale = (BigDecimal(b.volume - ingredient.volume) / BigDecimal(reactionIngredientToBoost.result.volume)).setScale(0, RoundingMode.CEILING).toBigInt
             val scaledUpReaction = Reaction(reactionIngredientToBoost.requirements.map { i => i.copy(volume = i.volume * scale) },
               Ingredient(reactionIngredientToBoost.result.volume * scale, reactionIngredientToBoost.result.name)
             )

             val (newIngredients, result) = useRecipe(scaledUpReaction, reactions, a._1)
             //println(s"newIngredients: $newIngredients")
             result match {
               case ReactionFailure => (newIngredients, result)
               case ReactionSuccess =>
                 val newIngredient = newIngredients.find(_.name == b.name).get

                 val newPot = newIngredients.removedAll(Set(newIngredient))
                 //println(s"New reacted pot: $newPot")

                 if (newIngredient.volume > b.volume)
                   (newPot.union(Set(Ingredient(newIngredient.volume - b.volume, b.name))), ReactionSuccess)
                 else if (newIngredient.volume < b.volume)
                   throw new Exception("Something has gone weird!")
                 else
                   (newPot, ReactionSuccess)
             }
           }
        } else
          (removed.union(Set(Ingredient(ingredient.volume - b.volume, b.name))), ReactionSuccess)
    }

    //add reaction to ingredients adding in new
    list._2 match {
      case ReactionFailure => list
      case ReactionSuccess =>
        (list._1.find(_.name == reaction.result.name)
          .map(i => list._1.removedAll(Set(i)).union(Set(Ingredient(reaction.result.volume + i.volume, i.name))))
          .getOrElse(list._1.union(Set(reaction.result))), ReactionSuccess)
    }
  }

  def findEachAvailableReaction(reactions:Seq[Reaction], ingredients: Set[Ingredient]): Seq[Reaction] =
    reactions.filter(r => !ingredients.exists(_.name == r.result.name) && r.requirements.forall(i => ingredients.exists(_.name == i.name)))

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
