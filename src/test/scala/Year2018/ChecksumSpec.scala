package Year2018

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ChecksumSpec extends AnyWordSpec with Matchers {

  "Doubles and Triples" ignore {
    "be zero for a string containing no duplicates" in {

      val input = "abcdef"
      Checksum.doublesAndTriples(input) shouldBe (0,0)

    }

    "be zero for a string containing one two letter and no three" in {

      val input = "abbcde"
      Checksum.doublesAndTriples(input) shouldBe (1,0)

    }

    "be zero for a string containing no two letter and one three" in {

      val input = "abcccd"
      Checksum.doublesAndTriples(input) shouldBe (0,1)

    }


    "be zero for a string containing two two letter and no three" in {

      val input = "aabcdd"
      Checksum.doublesAndTriples(input) shouldBe (1,0)

    }

    "be zero for a string containing one two letter (ee) and no three" in {

      val input = "abcdee"
      Checksum.doublesAndTriples(input) shouldBe (1,0)

    }

    "be zero for a string containing no two letter and two three" in {

      val input = "ababab"
      Checksum.doublesAndTriples(input) shouldBe (0,1)

    }

  }

  "Checksum" ignore {
    "correctly calculate a checksum" in {
      val input = Seq("abcdef","bababc", "abbcde","abcccd","aabcdd","abcdee","ababab")
      Checksum.checksum(input) shouldBe 12
    }
  }

  "hamming distance" ignore {
    "return an edit position" in  {
      Checksum.hammingDistance("abcde","axcye") shouldBe Seq(1,3)
    }
  }

  "which boxes differ by one edit" ignore {
    "return the boxes that differ by only one edit" in {
      val input = Seq("abcde","fghij","klmno","pqrst","fguij","axcye","wvxyz")
      Checksum.whichBoxesDifferByOneEdit(input) shouldBe Seq(("fghij","fguij"))
    }
  }

  "boxes with edited name" ignore {
    "return the corrected name with edit removed" in {
      val input = Seq(("fghij","fguij"))
      Checksum.boxesWithEditedName(input) shouldBe Seq("fgij")
    }
  }
}
