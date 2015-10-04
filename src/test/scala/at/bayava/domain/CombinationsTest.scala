package at.bayava.domain

import at.bayava.domain.Combinations._

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

    it("should match full house correctly") {
      val hands = Table(("hand", "isFullHouse"),
        ("Ac as ah ad 2d", false),
        ("Ac as 2h ad 2d", true),
        ("qc 2s ah ad 2d", false),
        ("2c 2s 3h 3d 2d", true),
        ("3c as 4h ad 2d", false)
      )
      forAll(hands) { (hand, isFullHouse) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[FullHouse] == isFullHouse)
      }
    }

    it("should match flush correctly") {
      val hands = Table(("hand", "isFlush"),
        ("Ad qd td qd jd", true),
        ("8c qs th kd jd", false),
        ("qc ts th kd jd", false),
        ("Ac qs th 9d jd", false),
        ("3c ad 4d 6d 2d", false),
        ("3c ac 6c 5c 2c", true)
      )
      forAll(hands) { (hand, isFlush) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[Flush] == isFlush)
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

    it("should match three of a kind correctly") {
      val hands = Table(("hand", "isThreeOfAKind"),
        ("Ac qs th kd jd", false),
        ("8c qs th kd jd", false),
        ("qc ts th kd jd", false),
        ("Ac ts th td jd", true),
        ("3c ac 4h 6s 2d", false),
        ("3c ac 4h 4s 4d", true)
      )
      forAll(hands) { (hand, isThreeOfAKind) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[ThreeOfAKind] == isThreeOfAKind)
      }
    }

    it("should match two pairs correctly") {
      val hands = Table(("hand", "isTwoPairs"),
        ("Ac as th td tc", false),
        ("Ac as 4h td tc", true),
        ("qc ts th kd kc", true),
        ("Ac ts th td jd", false),
        ("3c ac 4h 5s 2d", false),
        ("3c ac 4h 4s 4d", false)
      )
      forAll(hands) { (hand, isTwoPairs) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[TwoPairs] == isTwoPairs)
      }
    }

    it("should match a pair correctly") {
      val hands = Table(("hand", "isPair"),
        ("Ac as th td tc", false),
        ("Ac as 4h td tc", false),
        ("qc ts th 2d 5c", true),
        ("Ac 2s th 2d jd", true),
        ("3c ac 4h 5s 2d", false),
        ("3c ac 4h 4s 4d", false)
      )
      forAll(hands) { (hand, isPair) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[Pair] == isPair)
      }
    }

    it("should match a high card correctly") {
      val hands = Table(("hand", "isHighCard"),
        ("Ac as th td tc", false),
        ("Ac as 4h td tc", false),
        ("qc ts th 2d 5c", false),
        ("Ac 2s th 2d jd", false),
        ("3c ac 4h 5s 2d", false),
        ("3c ac Th 5s 2d", true),
        ("3c 6c Th 5s jd", true),
        ("3c 2c Th qs kd", true),
        ("3c ac 4h 4s 4d", false)
      )
      forAll(hands) { (hand, isHighCard) =>
        assert(Combinations.matchCombination(hand).isInstanceOf[HighCard] == isHighCard)
      }
    }


  }
}
