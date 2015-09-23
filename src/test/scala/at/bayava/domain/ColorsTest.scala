package at.bayava.domain

import at.bayava.domain.Colors.{CLUBS, DIAMONDS, HEARTHS, SPADES}
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

/**
 * Created by pbayer.
 */
@RunWith(classOf[JUnitRunner])
class ColorsTest extends FunSpec with PropertyChecks {

  describe("Colors") {
    describe("when calling the fromChar factory method") {
      it("should throw an IllegalArgumentException for unknown values") {
        intercept[IllegalArgumentException] {
          Colors.fromChar('X')
        }
      }

      val colors = Table(("input", "expected"),
        ('h', HEARTHS),
        ('H', HEARTHS),
        ('c', CLUBS),
        ('S', SPADES),
        ('d', DIAMONDS)
      )
      it("should return the appropriate Object") {
        forAll(colors) { (input: Char, expected: Colors.Color) =>
          assert(Colors.fromChar(input) == expected)
        }
      }
    }
  }

}