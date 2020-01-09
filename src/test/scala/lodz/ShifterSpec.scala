package lodz

import org.scalatest._

class ShifterSpec extends FlatSpec with Matchers {

  import Shifter._

  "A Shifter" should "shift as expected" in {
    shift("ABCDEF", 0) shouldBe "ABCDEF"
    shift("ABCDEF", 1) shouldBe "BCDEFG"
    shift("ABCDEF", 25) shouldBe "ZABCDE"
    shift("ABCDEF", 26) shouldBe "ABCDEF"
    shift("ABCDEF", -1) shouldBe "ZABCDE"
    shift("ABCDEF", -25) shouldBe "BCDEFG"
    shift("ABCDEF", -26) shouldBe "ABCDEF"
  }

  it should "convert lowercase to uppercase" in {
    shift("abcdef", 1) shouldBe "BCDEFG"
  }

  it should "fail if the count is outside [-26, 26]" in {
    a[AssertionError] should be thrownBy shift("ABCDEF", 27)
    a[AssertionError] should be thrownBy shift("ABCDEF", 27)
    a[AssertionError] should be thrownBy shift("ABCDEF", -27)
  }

  it should "fail if there are any non-alphabetic characters in the string" in {
    a[AssertionError] should be thrownBy shift("ABC.EF", 0)
  }
}