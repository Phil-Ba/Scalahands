package at.bayava.domain

import at.bayava.domain.Colors.Color
import at.bayava.domain.Values.Value

/**
 * Created by pbayer.
 */
class CardTest extends BaseScalahandsSpec {
  describe("Cards") {
    describe("apply method") {


      it("should throw an exception for illegal values") {
        val invalidApplyValues = Table("input", "12312", "1", "", null)

        forAll(invalidApplyValues) { (input) =>
          intercept[IllegalArgumentException] {
            Card(input)
          }
        }
      }

      it("should create the right cards for a string input using apply") {
        val applyValues = Table(("input", "color", "value")
          , ("8C", Colors.CLUBS, Values.EIGHT)
          , ("kd", Colors.DIAMONDS, Values.KING)
          , ("Qh", Colors.HEARTHS, Values.QUEEN)
          , ("3S", Colors.SPADES, Values.THREE)
          , ("ts", Colors.SPADES, Values.TEN)
          , ("9d", Colors.DIAMONDS, Values.NINE)
        )

        forAll(applyValues) { (input: String, color: Color, value: Value) =>
          val card = Card(input)
          assert(card.value == value)
          assert(card.color == color)
        }
      }
    }

    describe("should be equal") {
      it("if they have the same color and value") {
        val cardValues = Table("card", "2H", "tC", "As")

        forAll(cardValues) { card =>
          assert(Card(card) == Card(card))
          assert(Card(card.toLowerCase) == Card(card.toUpperCase))
        }
      }
    }

    describe("should not be equal") {
      it("if they have different color or values") {
        val cardValues = Table(("card1", "card2"),
          ("2S", "2H")
          , ("3C", "4C")
          , ("5h", "6H")
          , ("tc", "ts")
        )

        forAll(cardValues) { (card1: String, card2: String) =>
          assert(Card(card1) != Card(card2))
          assert(Card(card2) != Card(card1))
        }
      }
    }

    it("the card with the higher value should be considered as higher") {
      val cardValues = Table(("card1", "card2", "expected result"),
        ("2S", "2H", 0),
        ("3C", "4D", -1),
        ("4C", "3D", 1),
        ("5h", "6H", -1),
        ("tc", "5s", 1)
      )

      forAll(cardValues) { (card1, card2, expectedResult) =>
        assert(Card(card1).compareTo(Card(card2)).signum == expectedResult)
      }
    }

  }

}
