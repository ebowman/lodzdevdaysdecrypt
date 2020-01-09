package lodz

import org.scalatest._

class SignatureSpec extends FlatSpec with Matchers {

  import Shifter._

  "A Signature" should "sign as expected" in {
    signature("ABCDEF") shouldBe "0101010101"
    signature("FEDCBA") shouldBe "1919191919"
    signature("ABCDEF") shouldBe signature("BCDEFG")
    signature("ABCDEF") shouldBe signature("ZABCDE")
  }

  it should "sign in a way that is stable across rotations" in {
    val sig = signature("ABCDEF")
    for (s <- -26 to 26) {
      signature(shift("abcdef", s)) shouldBe sig
    }
  }
}