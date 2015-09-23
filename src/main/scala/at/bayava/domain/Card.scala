package at.bayava.domain

import at.bayava.domain.Colors.Color
import at.bayava.domain.Values.Value

/**
 * Created by pbayer.
 */
class Card(val value: Value, val color: Color) extends Ordered[Card] {

  override def compare(that: Card): Int = {
    value.compare(that.value)
  }


}

object Card {

  def apply(input: String): Card = {
    require(input != null && input.length == 2, "The input string must have exactly 2 characters!")
    new Card(Values.fromChar(input charAt 0), Colors.fromChar(input charAt 1))
  }

}
