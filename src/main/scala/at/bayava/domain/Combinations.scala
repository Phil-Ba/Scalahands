package at.bayava.domain

import at.bayava.domain.Values.{ACE, TWO, Value}

/**
 * Created by pbayer.
 */
object Combinations {

  sealed abstract class Combination(val hand: Hand, val rank: Int) extends Ordered[Combination] {

    override def compare(that: Combination): Int = {
      val compareByRank: Int = this.rank compareTo that.rank
      if (compareByRank != 0) {
        println("Combinations are different, comparing by rank!")
        compareByRank
      }
      else {
        println("Combinations are the same, comparing by pairs!")
        comparePairedCards(that)
      }
    }

    protected def comparePairedCards(that: Combination): Int = {
      val thisCardsByCount = hand.countValueGroups() groupBy (_._2) mapValues (_.keySet)
      val thatCardsByCount = that.hand.countValueGroups() groupBy (_._2) mapValues (_.keySet)
      val thisSortedKeys = thisCardsByCount.keySet.toList.sorted(Ordering[Int].reverse)
      val thatSortedKeys = thisCardsByCount.keySet.toList.sorted(Ordering[Int].reverse)
      for (index <- thisSortedKeys.indices) {
        val thisSortedValues = thisCardsByCount(thisSortedKeys(index)).toList.sorted(Ordering[Value].reverse)
        val thatSortedValues = thatCardsByCount(thatSortedKeys(index)).toList.sorted(Ordering[Value].reverse)
        for (indexValue <- thisSortedValues.indices) {
          val result = thisSortedValues(indexValue).compare(thatSortedValues(indexValue))
          if (result != 0) {
            println(s"Different pair value found: this='${thisSortedValues(indexValue)}' that='${thisSortedValues(indexValue)}'")
            return result
          }
        }
      }
      compareKickers(that)
    }

    protected def compareKickers(that: Combination): Int = {
      val sortedThisKickers: List[Card] = this.hand.kickers().sorted(Ordering[Card].reverse)
      val sortedThatKickers: List[Card] = that.hand.kickers().sorted(Ordering[Card].reverse)
      for (index <- sortedThisKickers.indices) {
        if (sortedThisKickers(index).value != sortedThatKickers(index).value) {
          println(s"Different kicker value found: this='${sortedThisKickers(index)}' that='${sortedThatKickers(index)}'")
          return sortedThisKickers(index) compare sortedThatKickers(index)
        }
      }
      0
    }

    override def toString: String = this.getClass.getSimpleName
  }

  class RoyalFlush(hand: Hand) extends Combination(hand, 9) {
  }

  object RoyalFlush {

    private def isFlush: Function[Hand, Boolean] = {
      case Flush(_) => true
      case _ => false
    }

    private def isStraight: Function[Hand, Boolean] = {
      case Straight(_) => true
      case _ => false
    }

    def unapply(hand: Hand): Option[RoyalFlush] = {
      if (isFlush(hand) && isStraight(hand)) Some(new RoyalFlush(hand)) else None
    }
  }

  class Poker(hand: Hand) extends Combination(hand, 8) {
  }

  object Poker {
    def unapply(hand: Hand): Option[Poker] = if (hand.countValueGroups().exists(_._2 == 4)) Some(new Poker(hand)) else None
  }

  class FullHouse(hand: Hand) extends Combination(hand, 7) {
  }

  object FullHouse {

    private def isThreeOfAKind: Function[Hand, Boolean] = {
      case ThreeOfAKind(_) => true
      case _ => false
    }

    private def isPair: Function[Hand, Boolean] = {
      case Pair(_) => true
      case _ => false
    }

    def unapply(hand: Hand): Option[FullHouse] = {
      if (isThreeOfAKind(hand) && isPair(hand)) Some(new FullHouse(hand)) else None
    }
  }

  class Flush(hand: Hand) extends Combination(hand, 6) {
  }

  object Flush {
    def unapply(hand: Hand): Option[Flush] = if (hand.countColorGroups().exists(_._2 == hand.cards.size)) Some(new Flush(hand)) else None
  }

  class Straight(hand: Hand) extends Combination(hand, 5) {
  }

  object Straight {
    def unapply(hand: Hand): Option[Straight] = {
      var min: Int = hand.cards.min(Ordering[Card]).value.ordinal
      val aceLow = if (min == TWO.ordinal && hand.cards.map(_.value.ordinal).contains(ACE.ordinal)) true else false

      if (aceLow) min = 1
      var sumOfStraight = min
      for (x <- 1 to hand.cards.size - 1) {
        sumOfStraight += x + min
      }

      var sumOfHand = hand.cards.map(_.value.ordinal).sum
      if (aceLow) {
        sumOfHand = sumOfHand - ACE.ordinal + 1
      }

      if (sumOfStraight == sumOfHand) Some(new Straight(hand)) else None
    }
  }

  class ThreeOfAKind(hand: Hand) extends Combination(hand, 4) {
  }

  object ThreeOfAKind {
    def unapply(hand: Hand): Option[ThreeOfAKind] = if (hand.countValueGroups().exists(_._2 == 3)) Some(new ThreeOfAKind(hand)) else None
  }

  class TwoPairs(hand: Hand) extends Combination(hand, 3) {
  }

  object TwoPairs {
    def unapply(hand: Hand): Option[TwoPairs] = if (hand.countValueGroups().values.count(_ == 2) == 2) Some(new TwoPairs(hand)) else None
  }

  class Pair(hand: Hand) extends Combination(hand, 2) {
  }

  object Pair {
    def unapply(hand: Hand): Option[Pair] = if (hand.countValueGroups().exists(_._2 == 2)) Some(new Pair(hand)) else None
  }

  class HighCard(hand: Hand) extends Combination(hand, 1) {
  }

  object HighCard {
    def unapply(hand: Hand): Option[HighCard] = Some(new HighCard(hand))
  }

  def matchCombination(handString: String): Combination = {

    matchCombination(Hand(handString))
  }

  def matchCombination(hand: Hand): Combination = {
    hand match {
      case RoyalFlush(royalFlush) => royalFlush
      case Poker(poker) => poker
      case FullHouse(fullHouse) => fullHouse
      case Flush(flush) => flush
      case Straight(straight) => straight
      case ThreeOfAKind(threeOfAKind) => threeOfAKind
      case TwoPairs(twoPairs) => twoPairs
      case Pair(pair) => pair
      case HighCard(highCard) => highCard
    }
  }

}
