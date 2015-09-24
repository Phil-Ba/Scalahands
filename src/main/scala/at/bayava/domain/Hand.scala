package at.bayava.domain

/**
 * Created by pbayer.
 */
class Hand(val cards: List[Card]) {

}

object Hand {
  def apply(input: String): Hand = {
    require(input != null)

    val cards: Array[String] = input.split(" ").to
    require(cards.size == 5)

    new Hand(cards.map {
      (card: String) => Card(card)
    }.toList)
  }
}