package at.bayava.domain

import at.bayava.domain.Colors.Color
import at.bayava.domain.Values.Value
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

/**
 * Created by pbayer.
 */
@RunWith(classOf[JUnitRunner])
class CardTest extends FunSpec with PropertyChecks {
  describe("Cards") {
    describe("apply method") {

      val invalidApplyValues = Table("input", "12312", "1", "", null)

      it("should throw an exception for illegal values") {
        forAll(invalidApplyValues) { (input) =>
          intercept[IllegalArgumentException] {
            Card(input)
          }
        }
      }

      val applyValues = Table(("input", "color", "value")
        , ("8C", Colors.CLUBS, Values.EIGHT)
        , ("kd", Colors.DIAMONDS, Values.KING)
        , ("Qh", Colors.HEARTHS, Values.QUEEN)
        , ("3S", Colors.SPADES, Values.THREE)
        , ("ts", Colors.SPADES, Values.TEN)
        , ("9d", Colors.DIAMONDS, Values.NINE)
      )
      it("should") {
        forAll(applyValues) { (input: String, color: Color, value: Value) =>
          val card = Card(input)
          assert(card.value == value)
          assert(card.color == color)
        }
      }

    }
  }

}
