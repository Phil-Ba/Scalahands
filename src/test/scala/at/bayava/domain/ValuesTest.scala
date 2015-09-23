package at.bayava.domain

import at.bayava.domain.Values._
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.PropertyChecks

/**
 * Created by pbayer.
 */
@RunWith(classOf[JUnitRunner])
class ValuesTest extends FunSpec with PropertyChecks {

  describe("Values") {
    describe("when calling the fromChar factory method") {
      it("should throw an IllegalArgumentException for unknown values") {
        intercept[IllegalArgumentException] {
          Colors.fromChar('X')
        }
      }

      val values = Table(("input", "expected"),
        ('9', NINE),
        ('3', THREE),
        ('T', TEN),
        ('Q', QUEEN),
        ('a', ACE)
      )
      it("should return the appropriate Object") {
        forAll(values) { (input: Char, expected: Values.Value) =>
          assert(Values.fromChar(input) == expected)
        }
      }
    }
  }
}
