package at.bayava.domain

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

      val applyValues = Table("input", "12312", "1", "", null)

      it("should throw an exception for illegal values") {
        forAll(applyValues) { (input) =>
          intercept[IllegalArgumentException] {
            Card(input)
          }
        }
      }
    }
  }

}
