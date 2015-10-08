package at.bayava.domain

import at.bayava.domain.Combinations._
import org.scalatest.prop.TableFor3

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

  it("High card should compare correctly") {
    val hands = Table(("this", "that", "expected"),
      ("Ac as th td tc", "Ad ac ts th ts", 0),
      ("Ac as 4h td tc", "Ac as th 9d tc", -1),
      ("2c 3s 4h td 9c", "5c 4s 7h 8d 9c", 1),
      ("Ad kh qh jd 7c", "Ac ks qc jd 9d", -1)
    )
    forAll(hands) { (thisHand, thatHand, expected) =>
      val thisPair = new Pair(Hand(thisHand))
      val thatPair = new Pair(Hand(thatHand))
      assert(thisPair.compareTo(thatPair).signum == expected)
      assert(thatPair.compareTo(thisPair).signum == -expected)
    }
  }

  describe("Pairs") {
    it("should primarily compared by the pairs") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as th 9d 8c", "Ad ac ts 9h 8s", 0),
        ("Ac as th 9d 8c", "qd qc as 9h 8s", 1),
        ("4c as 4h 5d tc", "2c 5s 5h 4d 3c", -1)
      )

      assertCompareResult(hands, {
        new Pair(_)
      })
    }

    it("and then by their kickers") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as th 9d 8c", "Ad ac ts 7h 8s", 1),
        ("Ac qs th qd 8c", "qd qc 2s 9h 8s", 1),
        ("4c 2s 5h td tc", "5c ts th 4d 3c", -1)
      )

      assertCompareResult(hands, {
        new Pair(_)
      })
    }
  }

  describe("Two Pairs") {
    it("should primarily compared by the pairs") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as th td 8c", "Ad ac ts th 8s", 0),
        ("kc ks th td 8c", "qd qc js jh as", 1),
        ("kc ks qh qd 8c", "kd kc js jh as", 1)
      )

      assertCompareResult(hands, {
        new TwoPairs(_)
      })
    }

    it("and then by their kickers") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as th td 9c", "Ad ac ts th 8s", 1),
        ("qc qs th td 8c", "td tc qs qh as", -1),
        ("Ac qs ah td qc", "qd ac 2s ah qs", 1)
      )

      assertCompareResult(hands, {
        new TwoPairs(_)
      })
    }
  }

  describe("Three of a kind") {
    it("should primarily compared by the pairs") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as th ad 8c", "Ad ac as th 8s", 0),
        ("kc ts th td 8c", "8d 8c js 8h as", 1),
        ("kc 8s 8h qd 8c", "2d jc js jh 3d", -1)
      )

      assertCompareResult(hands, {
        new ThreeOfAKind(_)
      })
    }

    it("and then by their kickers") {
      val hands = Table(("this", "that", "expected"),
        ("Ac ts th td 9c", "Ad tc ts th 8s", 1),
        ("qc qs qh td 8c", "td qc qs qh as", -1),
        ("Ac as ah td kc", "qd ac as ah js", 1)
      )

      assertCompareResult(hands, {
        new ThreeOfAKind(_)
      })
    }
  }

  describe("Straights") {
    it("should be compared by high card") {
      val hands = Table(("this", "that", "expected"),
        ("Ac qs kh td jc", "Ad tc qs jh ks", 0),
        ("kc ts 9h qd jc", "ad 2c 4s 3h 5s", 1),
        ("tc 8s 9h 7d 6c", "9d jc ts 8h 7d", -1)
      )

      assertCompareResult(hands, {
        new Straight(_)
      })
    }
  }

  describe("Full Houses") {
    it("should primarily compared by the pairs") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as th td tc", "Ad ac ts th ts", 0),
        ("kc ts th td kc", "8d jc js 8h js", -1),
        ("8c ts th 8d 8c", "8d 8c 9s 8h 9s", 1),
        ("ac as th td tc", "td tc js jh jd", -1)
      )

      assertCompareResult(hands, {
        new FullHouse(_)
      })
    }
  }

  describe("Flushes") {
    it("should primarily compared by the high card") {
      val hands = Table(("this", "that", "expected"),
        ("As 4s ts 3s 5s", "Ad 4d 3d td 5d", 0),
        ("8s 4s ts 3s 5s", "Ad 4d 3d td 5d", -1),
        ("As 4s ts 3s 5s", "jd 4d 3d td 5d", 1)
      )

      assertCompareResult(hands, {
        new Flush(_)
      })
    }
  }

  describe("Pokers") {
    it("should primarily compared by the pairs") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as ah ad 8c", "Ad ac as ah 8d", 0),
        ("tc ts th td 7c", "8d 8c 8s 8h as", 1)
      )

      assertCompareResult(hands, {
        new Poker(_)
      })
    }

    it("and then by their kickers") {
      val hands = Table(("this", "that", "expected"),
        ("Ac as ah ad 9c", "Ad ac as ah 8s", 1),
        ("qc qs qh qd 8c", "qd qc qs qh as", -1)
      )

      assertCompareResult(hands, {
        new Poker(_)
      })
    }
  }

  describe("Royal Flushes") {
    it("should be compared by high card") {
      val hands = Table(("this", "that", "expected"),
        ("Ac qc kc tc jc", "Ad td qd jd kd", 0),
        ("kc tc 9c qc jc", "ad 2d 4d 3d 5d", 1),
        ("th 8h 9h 7h 6h", "9s js ts 8s 7s", -1)
      )

      assertCompareResult(hands, {
        new RoyalFlush(_)
      })
    }
  }

  def assertCompareResult(hands: TableFor3[String, String, Int], constructor: Hand => Combination) = {
    forAll(hands) { (thisHand, thatHand, expected) =>
      val thisCombination = constructor(Hand(thisHand))
      val thatCombination = constructor(Hand(thatHand))
      assert(thisCombination.compareTo(thatCombination).signum == expected)
      assert(thatCombination.compareTo(thisCombination).signum == -expected)
    }
  }

}
