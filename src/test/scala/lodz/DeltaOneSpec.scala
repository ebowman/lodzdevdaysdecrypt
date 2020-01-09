package lodz

import org.scalatest._

class DeltaOneSpec extends FlatSpec with Matchers {

  import Shifter._

  "A DeltaOne" should "delta as expected" in {
    deltaOne('A', 'B') shouldBe 1
    deltaOne('B', 'A') shouldBe 25
    deltaOne('Z', 'A') shouldBe 1
    deltaOne('A', 'Z') shouldBe 25
  }
}