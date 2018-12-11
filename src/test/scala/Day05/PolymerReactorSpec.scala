package Day05

import org.scalatest.{Matchers, WordSpec}

class PolymerReactorSpec extends WordSpec with Matchers {

  "PolymerReactor" should {


    "react to destroy each other" in {
       val polyString = "aA"

      PolymerReactor.scanAndReactPolymer(polyString) shouldBe ""
    }

    "not react when there is nothing to react" in {
      val polyString = "abAB"

      PolymerReactor.scanAndReactPolymer(polyString) shouldBe "abAB"
    }

    "react in two stages when new pairs are created from deletion" in {
      val polyString = "abBA"
      PolymerReactor.scanAndReactPolymer(polyString) shouldBe ""
    }

    "matching polarities are unreactive" in {
      val polyString = "aabAAB"
      PolymerReactor.scanAndReactPolymer(polyString) shouldBe "aabAAB"
    }

    "a mutistage reaction will still react" in {
      val polyString = "dabAcCaCBAcCcaDA"
      PolymerReactor.scanAndReactPolymer(polyString) shouldBe "dabCBAcaDA"
    }

    "count the correct number of unreacted units" in {
      val polyString = "dabAcCaCBAcCcaDA"
      PolymerReactor.countUnreactedPolymers(polyString) shouldBe 10
    }
  }

  "areSameLetter" should {
    "match letters which are the same but of a different case" in {
      PolymerReactor.areSameLetter('a', 'A') shouldBe true
    }

    "not match different letters" in {
      PolymerReactor.areSameLetter('b', 'A') shouldBe false
    }
  }
}
