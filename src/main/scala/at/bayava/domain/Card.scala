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

  override def toString: String = s"Card(value: $value , color: $color)"

  override def equals(other: Any): Boolean = other match {
    case that: Card =>
      value == that.value &&
        color == that.color
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(value, color)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object Card {

  def apply(input: String): Card = {
    require(input != null && input.length == 2, "The input string must have exactly 2 characters!")
    new Card(Values.fromChar(input charAt 0), Colors.fromChar(input charAt 1))
  }

}
