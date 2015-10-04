package at.bayava.domain

import at.bayava.domain.Combinations.{Poker, RoyalFlush, Straight}

/**
 * Created by pbayer.
 */
class CombinationsTest extends BaseScalahandsSpec {

  describe("Combinations") {

    it("should match royal flush correctly") {
      val hands = Table(("hand", "isRoyalFlush"),
        ("Ac 2c 3c 4c 5c", true),
        ("Ac 2d 3c 4c 5d", false),
        ("Ac tc qc 3c 4c", false),
        ("2c 2s 3h ad 2d", false),
        ("Ac qc kc tc jc", true)
      )
      forAll(hands) { (hand, isRoyalFlush) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[RoyalFlush] == isRoyalFlush)
      }
    }

    it("should match poker correctly") {
      val hands = Table(("hand", "isPoker"),
        ("Ac as ah ad 2d", true),
        ("Ac as 2h ad 2d", false),
        ("2c 2s 2h ad 2d", true),
        ("2c 2s 3h ad 2d", false),
        ("3c as 4h ad 2d", false)
      )
      forAll(hands) { (hand, isPoker) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[Poker] == isPoker)
      }
    }

    it("should match straight correctly") {
      val hands = Table(("hand", "isStraight"),
        ("Ac qs th kd jd", true),
        ("8c qs th kd jd", false),
        ("qc ts th kd jd", false),
        ("Ac qs th 9d jd", false),
        ("3c ac 4h 6s 2d", false),
        ("3c ac 4h 5s 2d", true)
      )
      forAll(hands) { (hand, isStraight) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[Straight] == isStraight)
      }
    }

  }
}
