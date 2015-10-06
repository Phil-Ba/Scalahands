package at.bayava.domain

import at.bayava.domain.Colors.Color
import at.bayava.domain.Values.Value

/**
 * Created by pbayer.
 */
class Hand(val cards: List[Card]) {

  def kickers: List[Card] = {
    cards groupBy (_.value) collect { case single if single._2.size == 1 => single._2.head } toList
  }

  def countValueGroups: Map[Value, Int] = {
    cards groupBy {
      _.value
    } mapValues {
      _.size
    } filter {
      _._2 > 1
    }
  }

  def countColorGroups: Map[Color, Int] = {
    cards groupBy {
      _.color
    } mapValues {
      _.size
    } filter {
      _._2 > 1
    }
  }

  override def toString: String = super.toString + " : " + cards.toString()
}

object Hand {
  def apply(input: String): Hand = {
    require(input != null)

    val cards = input.split(" ").toList
    require(cards.length == 5)
    new Hand(cards collect { case s: String => Card(s) })
  }
}