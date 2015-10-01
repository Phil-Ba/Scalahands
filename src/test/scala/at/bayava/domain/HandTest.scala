package at.bayava.domain

import at.bayava.domain.Colors.{CLUBS, DIAMONDS, HEARTHS, SPADES}
import at.bayava.domain.Values.{ACE, JACK, QUEEN}

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

    describe("countValueGroups") {

      it("should return the count of the values") {
        val params = Table(("cards", "groups"),
          ("AD KS QH JC JD", Map(JACK -> 2)),
          ("AD AS AH JC JD", Map(JACK -> 2, ACE -> 3)),
          ("AD qS QH JC JD", Map(JACK -> 2, QUEEN -> 2))
        )

        forAll(params) { (cards, result) =>
          val hand = Hand(cards)
          assert(hand.countValueGroups() == result)
        }
      }

      it("the result should not contain single values") {
        val params = Table(("cards", "groups"),
          ("AD KS QH TC JD", Map.empty),
          ("AD 5S 2H 3C JD", Map.empty),
          ("6D qS tH JC 9D", Map.empty)
        )

        forAll(params) { (cards, result) =>
          val hand = Hand(cards)
          assert(hand.countValueGroups() == result)
        }
      }
    }

    describe("countColorGroups") {

      it("should return the count of the colors") {
        val params = Table(("cards", "groups"),
          ("AD KS QH JC JD", Map(DIAMONDS -> 2)),
          ("AD AD AH JC JC", Map(DIAMONDS -> 2, CLUBS -> 2)),
          ("AD qd QH Jh Jd", Map(DIAMONDS -> 3, HEARTHS -> 2))
        )

        forAll(params) { (cards, result) =>
          val hand = Hand(cards)
          assert(hand.countColorGroups() == result)
        }
      }

      it("the result should not contain single colors") {
        val params = Table(("cards", "notInList"),
          ("AD KS QH TC JD", List(SPADES, CLUBS, HEARTHS)),
          ("AD 5D 2H 3C Jc", List(HEARTHS))
        )

        forAll(params) { (cards, notInList) =>
          val hand = Hand(cards)
          val colors = hand.countColorGroups()
          for (color <- notInList) {
            assert(colors.contains(color) == false)
          }
        }
      }
    }

  }

}
