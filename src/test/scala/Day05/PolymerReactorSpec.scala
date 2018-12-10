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

      PolymerReactor.scanAndReactPolymer(polyString) shouldBe ""
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
