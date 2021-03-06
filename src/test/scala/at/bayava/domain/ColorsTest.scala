package at.bayava.domain

import at.bayava.domain.Colors.{CLUBS, DIAMONDS, HEARTHS, SPADES}

/**
 * Created by pbayer.
 */
class ColorsTest extends BaseScalahandsSpec {

  describe("Colors") {
    describe("when calling the fromChar factory method") {
      it("should throw an IllegalArgumentException for unknown values") {
        intercept[IllegalArgumentException] {
          Colors('X')
        }
      }

      it("should return the appropriate Object") {
        val colors = Table(("input", "expected"),
          ('h', HEARTHS),
          ('H', HEARTHS),
          ('c', CLUBS),
          ('S', SPADES),
          ('d', DIAMONDS)
        )

        forAll(colors) { (input: Char, expected: Colors.Color) =>
          assert(Colors(input) == expected)
        }
      }
    }
  }

}
