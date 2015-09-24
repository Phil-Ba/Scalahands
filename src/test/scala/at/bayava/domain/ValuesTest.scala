package at.bayava.domain

import at.bayava.domain.Values._

/**
 * Created by pbayer.
 */
class ValuesTest extends BaseScalahandsSpec {

  describe("Values") {
    describe("when calling the fromChar factory method") {
      it("should throw an IllegalArgumentException for unknown values") {
        intercept[IllegalArgumentException] {
          Colors.fromChar('X')
        }
      }

      it("should return the appropriate Object") {
        val values = Table(("input", "expected"),
          ('9', NINE),
          ('3', THREE),
          ('T', TEN),
          ('Q', QUEEN),
          ('a', ACE),
          ('A', ACE)
        )

        forAll(values) { (input: Char, expected: Values.Value) =>
          assert(Values.fromChar(input) == expected)
        }
      }
    }
  }
}
