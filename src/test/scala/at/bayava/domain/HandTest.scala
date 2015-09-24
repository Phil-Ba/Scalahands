package at.bayava.domain

/**
 * Created by pbayer.
 */
class HandTest extends BaseScalahandsSpec {

  describe("The Hand") {
    describe("string apply method") {
      it("should throw an exception for illegal parameters") {
        val illegalParams = Table("illegalParam", "", "null", "AD", "AD KS QH JC", "AD KS QH JC JD JH")

        forAll(illegalParams) { (illegalParam: String) =>
          intercept[IllegalArgumentException] {
            Hand(illegalParam)
          }
        }
      }

      it("should contain all the cards passed as param") {
        val params = Table("cards", "AD KS QH JC JD", "2c tS 7H 3h jD")

        forAll(params) { (param: String) =>
          val hand = Hand(param)
          param.split(" ").foreach((card: String) =>
            assert(hand.cards.contains(Card(card)))
          )
        }
      }
    }
  }

}
